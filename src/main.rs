use argh::FromArgs;
use axum::{
    Router,
    extract::{Path as PathExtract, RawQuery, State},
    http::{HeaderMap, HeaderValue, StatusCode},
    response::{Html, IntoResponse},
    routing::get,
};
use mimalloc::MiMalloc;
use std::{
    collections::{BTreeMap, BTreeSet},
    fmt::Write,
    net::SocketAddr,
    path::{Path, PathBuf},
    sync::Arc,
    time::Duration,
};
use tokio::{
    fs::File,
    io::{AsyncBufReadExt, BufReader},
    net::TcpListener,
    sync::RwLock,
    task::spawn_blocking,
    time::sleep,
};
use warc::{WarcHeader, WarcReader};

#[global_allocator]
static GLOBAL: MiMalloc = MiMalloc;

/// serve warc files on http
#[derive(Debug, FromArgs)]
#[argh(help_triggers("-h", "--help"))]
struct Opt {
    /// socket address to bind
    #[argh(option, short = 'b', default = "\"[::1]:0\".parse().unwrap()")]
    bind: SocketAddr,
    /// seconds to wait between rereading cdx files
    #[argh(option, default = "300")]
    interval: u64,
    /// path(s) to directory of warc and cdx files
    #[argh(positional)]
    directory: Vec<PathBuf>,
}

#[derive(Debug)]
struct AppState {
    directories: Vec<PathBuf>,
    cdx_map: RwLock<BTreeMap<String, BTreeMap<u64, Arc<PathBuf>>>>,
    log: RwLock<String>,
}

impl AppState {
    async fn get_warc_response(&self, req_url: String, timestamp: u64) -> Option<WarcResponse> {
        let path = self
            .cdx_map
            .read()
            .await
            .get(&req_url)
            .and_then(|b| b.range(..=timestamp).next_back())
            .map(|(_, p)| p.clone())?;

        let buffered = spawn_blocking(move || read_warc_record(&req_url, &path))
            .await
            .ok()??;

        let response = buffered.body();
        let mut headers = [httparse::EMPTY_HEADER; 256];
        let mut res = httparse::Response::new(&mut headers);
        let Ok(httparse::Status::Complete(body_offset)) = res.parse(response) else {
            return None;
        };

        let content_type = res
            .headers
            .iter()
            .find(|h| h.name.eq_ignore_ascii_case("content-type"))
            .map(|h| h.value.to_vec())
            .and_then(|v| String::from_utf8(v).ok());
        let body = &response[body_offset..];

        Some(WarcResponse {
            code: res.code.unwrap_or(200),
            content_type,
            body: body.to_vec(),
        })
    }
}

fn read_warc_record(req_url: &str, path: &Path) -> Option<warc::Record<warc::BufferedBody>> {
    let mut file = WarcReader::from_path_gzip(path).ok()?;
    let mut stream_iter = file.stream_records();
    while let Some(Ok(record)) = stream_iter.next_item() {
        if record
            .header(WarcHeader::WarcType)
            .is_none_or(|t| t != "response")
        {
            continue;
        }
        let Some(target) = record.header(WarcHeader::TargetURI) else {
            continue;
        };
        let target = target.strip_prefix("<").unwrap_or(&target);
        let target = target.strip_suffix(">").unwrap_or(target);
        if target != req_url {
            continue;
        }
        return record.into_buffered().ok();
    }

    None
}

#[derive(Debug)]
struct WarcResponse {
    code: u16,
    content_type: Option<String>,
    body: Vec<u8>,
}

fn escape(inp: &str) -> String {
    let mut out = String::new();
    for c in inp.chars() {
        out.push_str(match c {
            '<' => "&lt;",
            '>' => "&gt;",
            '&' => "&amp;",
            '"' => "&quot;",
            '\'' => "&apos;",
            _ => {
                out.push(c);
                continue;
            }
        });
    }
    out
}

async fn root(State(state): State<Arc<AppState>>) -> Html<String> {
    let log = state.log.read().await;
    Html(format!(
        "<!DOCTYPE html>
<h1>wahs</h1>
go request stuff to <code>/YYYYMMDDHHMMSS/someurl</code>
(remember to url escape the url)
<h2>log</h2>
<pre>{}</pre>",
        escape(&log)
    ))
}

fn process_timestamp(inp: &str) -> u64 {
    let mut numbers = inp.chars().take_while(|c| c.is_ascii_digit());
    let mut num = 0;
    for _ in 0..14 {
        num *= 10;
        num += numbers.next().map(|i| i as u64 - '0' as u64).unwrap_or(9);
    }
    num
}

async fn from_warc(
    State(state): State<Arc<AppState>>,
    PathExtract((timestamp, path_url)): PathExtract<(String, String)>,
    RawQuery(query): RawQuery,
) -> impl IntoResponse {
    let mut requested_url = path_url;
    if let Some(query) = query {
        requested_url.push('?');
        requested_url.push_str(&query);
    }
    let timestamp = process_timestamp(&timestamp);
    let mut headers = HeaderMap::new();
    let Some(response) = state.get_warc_response(requested_url, timestamp).await else {
        headers.insert("content-type", HeaderValue::from_static("text/html"));
        return (
            StatusCode::NOT_FOUND,
            headers,
            b"<!DOCTYPE html>
<h1>knot found</h1>
if you know this is something that should be in a warc file for the specified time range,
try again in a few minutes, it might just need to be indexed.
make sure you url encoded the provided url and be mindful of trailing slashes,
wahs does not canonicalize it."
                .to_vec(),
        );
    };

    if let Some(content_type) = response.content_type
        && let Ok(h) = HeaderValue::from_str(&content_type)
    {
        headers.insert("content-type", h);
    }

    (
        StatusCode::from_u16(response.code).unwrap_or(StatusCode::OK),
        headers,
        response.body,
    )
}

async fn read_cdx(
    cdxname: &Path,
    map: &mut BTreeMap<String, BTreeMap<u64, Arc<PathBuf>>>,
    dedup: &mut BTreeSet<Arc<PathBuf>>,
) -> Result<(), &'static str> {
    let parent = cdxname.parent().ok_or("no parent")?;
    let file = BufReader::new(
        File::open(cdxname)
            .await
            .map_err(|_| "could not open file")?,
    );
    let mut lines = file.lines();

    let Ok(Some(header)) = lines.next_line().await else {
        return Err("could not read cdx header");
    };
    let mut header = header
        .split(' ')
        .skip_while(|&s| s != "CDX")
        .skip(1)
        .enumerate();
    let (url, _) = header
        .clone()
        .find(|(_, c)| *c == "a")
        .ok_or("no `a` url column in header")?;
    let (date, _) = header
        .clone()
        .find(|(_, c)| *c == "b")
        .ok_or("no `b` date column in header")?;
    let (warcname, _) = header
        .find(|(_, c)| *c == "g")
        .ok_or("no `g` warc filename column in header")?;

    while let Some(line) = lines
        .next_line()
        .await
        .map_err(|_| "bork while reading line")?
    {
        let columns: Vec<_> = line.split(' ').collect();
        let &url = columns.get(url).ok_or("url field missing")?;
        let date = columns.get(date).ok_or("date field missing")?;
        let date = date.parse().map_err(|_| "malformed date")?;
        let &warcname = columns.get(warcname).ok_or("warc filename field missing")?;
        let warcname = parent.join(warcname);
        let warcname = if let Some(w) = dedup.get(&warcname) {
            w.clone()
        } else {
            let w = Arc::new(warcname);
            dedup.insert(w.clone());
            w
        };

        map.entry(url.to_string())
            .or_default()
            .insert(date, warcname);
    }

    Ok(())
}

async fn reindex(dirs: &[PathBuf]) -> (BTreeMap<String, BTreeMap<u64, Arc<PathBuf>>>, String) {
    let mut dedup = BTreeSet::new();
    let mut map = BTreeMap::new();
    let mut log = String::new();

    for dirname in dirs {
        let mut dir = match tokio::fs::read_dir(dirname).await {
            Ok(d) => d,
            Err(e) => {
                writeln!(log, "could not index directory {dirname:?}: {e}").unwrap();
                continue;
            }
        };

        while let Ok(Some(entry)) = dir.next_entry().await {
            let name = entry.path();
            if name
                .extension()
                .is_none_or(|e| !e.eq_ignore_ascii_case("cdx"))
            {
                continue;
            }

            if let Err(e) = read_cdx(&name, &mut map, &mut dedup).await {
                writeln!(log, "could not index {name:?}: {e}").unwrap();
                continue;
            }

            writeln!(log, "indexed {name:?}").unwrap();
        }

        writeln!(log, "indexed directory {dirname:?}").unwrap();
    }

    write!(log, "finished indexing {} urls!", map.len()).unwrap();
    (map, log)
}

async fn reindex_forever(state: Arc<AppState>, interval: u64) {
    let interval = Duration::from_secs(interval);
    loop {
        let (map, stat) = reindex(&state.directories).await;
        {
            let mut cdx_map = state.cdx_map.write().await;
            *cdx_map = map;
            let mut log = state.log.write().await;
            *log = stat;
        }
        sleep(interval).await;
    }
}

#[tokio::main]
async fn main() {
    let opt: Opt = argh::from_env();

    let mut directories = opt.directory;
    if directories.is_empty() {
        directories.push(".".into());
    }
    let state = Arc::new(AppState {
        directories,
        cdx_map: RwLock::new(BTreeMap::new()),
        log: RwLock::new("not indexed yet...".to_string()),
    });

    tokio::task::spawn(reindex_forever(state.clone(), opt.interval));

    let app = Router::new()
        .route("/", get(root))
        .route("/{timestamp}/{*pathurl}", get(from_warc))
        .with_state(state);

    let listen = TcpListener::bind(opt.bind).await.unwrap();
    eprintln!("listening on {}", listen.local_addr().unwrap());
    axum::serve(listen, app.into_make_service()).await.unwrap();
}
