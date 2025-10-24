use argh::FromArgs;
use axum::{
    Router,
    extract::{Path as PathExtract, RawQuery, State},
    http::{HeaderMap, HeaderName, HeaderValue, StatusCode},
    response::{Html, IntoResponse},
    routing::get,
};
use lol_html::{HtmlRewriter, element};
use mimalloc::MiMalloc;
use percent_encoding::{AsciiSet, CONTROLS, utf8_percent_encode};
use std::{
    collections::{BTreeMap, BTreeSet},
    fmt::{self, Write},
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
use url::Url;
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

const URL_UNSAFE: &AsciiSet = &CONTROLS
    .add(b' ')
    .add(b'"')
    .add(b'%')
    .add(b'<')
    .add(b'>')
    .add(b'\\')
    .add(b'^')
    .add(b'`')
    .add(b'{')
    .add(b'|')
    .add(b'}');

#[derive(Debug)]
struct AppState {
    directories: Vec<PathBuf>,
    cdx_map: RwLock<BTreeMap<String, BTreeMap<u64, Arc<PathBuf>>>>,
    log: RwLock<String>,
}

impl AppState {
    async fn get_warc_response(
        &self,
        req_url: String,
        timestamp: u64,
    ) -> Result<WarcResponse, ResponseError> {
        let base_url = Url::parse(&req_url).map_err(ResponseError::UrlParse)?;
        let path = self
            .cdx_map
            .read()
            .await
            .get(&req_url)
            .and_then(|b| b.range(..=timestamp).next_back())
            .map(|(_, p)| p.clone())
            .ok_or(ResponseError::NotFound)?;

        let buffered = spawn_blocking(move || read_warc_record(&req_url, &path))
            .await
            .map_err(ResponseError::TokioJoin)??;

        let response = buffered.body();
        let mut headers = [httparse::EMPTY_HEADER; 256];
        let mut res = httparse::Response::new(&mut headers);
        let Ok(httparse::Status::Complete(body_offset)) = res.parse(response) else {
            return Err(ResponseError::HttpParse);
        };

        let content_type = res
            .headers
            .iter()
            .find(|h| h.name.eq_ignore_ascii_case("content-type"))
            .map(|h| h.value.to_vec())
            .and_then(|v| String::from_utf8(v).ok())
            .unwrap_or_else(|| "text/html".to_string());
        let body = &response[body_offset..];

        let ct = content_type
            .split_once(';')
            .map(|(s, _)| s)
            .unwrap_or(&content_type);
        // FIXME: treating xhtml like html is very naughty
        // people are usually nice enough to make their xhtml
        // html-compatible-ish tho
        let body = if ct.eq_ignore_ascii_case("text/html")
            || ct.eq_ignore_ascii_case("application/xhtml+xml")
        {
            let mut output = vec![];
            let mut rewriter = HtmlRewriter::new(
                lol_html::Settings {
                    element_content_handlers: vec![element!("[href]", |el| {
                        let Some(href) = el.get_attribute("href") else {
                            return Ok(());
                        };
                        let Some(url) = mangle_url(&base_url, &href, timestamp) else {
                            return Ok(());
                        };
                        _ = el.set_attribute("href", &url);
                        Ok(())
                    })],
                    ..lol_html::Settings::new()
                },
                |c: &[u8]| output.extend_from_slice(c),
            );

            rewriter.write(body).map_err(ResponseError::RewriteHtml)?;
            rewriter.end().map_err(ResponseError::RewriteHtml)?;

            output
        } else {
            body.to_vec()
        };

        let mut headers = HeaderMap::new();
        for h in res.headers.iter() {
            if let Ok(k) = HeaderName::try_from(format!("x-archive-orig-{}", h.name))
                && let Ok(v) = HeaderValue::from_bytes(h.value)
            {
                headers.insert(k, v);
            }
        }
        if let Ok(h) = HeaderValue::from_str(&content_type) {
            headers.insert("content-type", h);
        }
        if let Some(location) = res
            .headers
            .iter()
            .find(|h| h.name.eq_ignore_ascii_case("location"))
            .and_then(|h| str::from_utf8(h.value).ok())
            && let Some(mangled) = mangle_url(&base_url, location, timestamp)
            && let Ok(h) = HeaderValue::from_str(&mangled)
        {
            headers.insert("location", h);
        }

        Ok(WarcResponse {
            code: res.code.unwrap_or(200),
            headers,
            body,
        })
    }
}

fn mangle_url(base: &Url, join: &str, timestamp: u64) -> Option<String> {
    let url = base.join(join).ok()?;
    let enc = utf8_percent_encode(url.as_str(), URL_UNSAFE);
    Some(format!("/{timestamp}/{enc}"))
}

fn read_warc_record(
    req_url: &str,
    path: &Path,
) -> Result<warc::Record<warc::BufferedBody>, ResponseError> {
    let mut file = WarcReader::from_path_gzip(path).map_err(ResponseError::OpenWarc)?;
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
        return record.into_buffered().map_err(ResponseError::RecordBody);
    }

    Err(ResponseError::WarcMissing)
}

#[derive(Debug)]
struct WarcResponse {
    code: u16,
    headers: HeaderMap,
    body: Vec<u8>,
}

#[derive(Debug)]
enum ResponseError {
    UrlParse(url::ParseError),
    NotFound,
    TokioJoin(tokio::task::JoinError),
    HttpParse,
    OpenWarc(std::io::Error),
    RecordBody(std::io::Error),
    WarcMissing,
    RewriteHtml(lol_html::errors::RewritingError),
}

impl fmt::Display for ResponseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "<!DOCTYPE html>")?;
        match self {
            Self::UrlParse(parse_error) => write!(
                f,
                "<h1>could not parse provided url</h1>{}",
                escape(&parse_error.to_string())
            ),
            Self::NotFound => write!(
                f,
                "<h1>knot found</h1>
if you know this is something that should be in a warc file for the specified time range,
try again in a few minutes, the cdx listing it might just need to be indexed.
make sure you url encoded the provided url and be mindful of trailing slashes,
wahs does not canonicalize it."
            ),
            Self::TokioJoin(join_error) => write!(
                f,
                "<h1>what the tokio doin</h1>{}",
                escape(&join_error.to_string())
            ),
            Self::HttpParse => write!(
                f,
                "<h1>could not parse http response in warc</h1>
... this warc file does have http stuff in it, right?"
            ),
            Self::OpenWarc(error) => write!(
                f,
                "<h1>could knot open warc</h1>{}",
                escape(&error.to_string())
            ),
            Self::RecordBody(error) => write!(
                f,
                "<h1>could not read warc record body</h1>{}",
                escape(&error.to_string())
            ),
            Self::WarcMissing => write!(
                f,
                "<h1>warc record missing</h1>
your cdx file says its in there, it or the warc file may be corrupted :("
            ),
            Self::RewriteHtml(rewrite_error) => write!(
                f,
                "<h1>error rewriting html</h1>{}",
                escape(&rewrite_error.to_string())
            ),
        }
    }
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
    let response = match state.get_warc_response(requested_url, timestamp).await {
        Ok(r) => r,
        Err(e) => {
            let mut headers = HeaderMap::new();
            headers.insert("content-type", HeaderValue::from_static("text/html"));
            return (StatusCode::NOT_FOUND, headers, e.to_string().into_bytes());
        }
    };

    (
        StatusCode::from_u16(response.code).unwrap_or(StatusCode::OK),
        response.headers,
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
