use argh::FromArgs;
use axum::{
    extract::{Path as PathExtract, RawQuery, State},
    response::Html,
    response::IntoResponse,
    routing::get,
    Router,
};
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
    time::sleep,
};

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
    format!("{requested_url} at {timestamp}")
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
