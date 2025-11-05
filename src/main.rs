use argh::FromArgs;
use axum::{
    Router,
    extract::{Path as PathExtract, Query, RawQuery, State},
    http::{HeaderMap, HeaderName, HeaderValue, StatusCode, Uri},
    response::{Html, IntoResponse},
    routing::get,
};
use html_escape::{decode_html_entities, encode_double_quoted_attribute, encode_text};
use libflate::gzip::MultiDecoder as GzipReader;
use lol_html::{HtmlRewriter, element};
use mimalloc::MiMalloc;
use percent_encoding::{AsciiSet, CONTROLS, percent_decode_str, utf8_percent_encode};
use std::{
    borrow::Cow,
    collections::{BTreeMap, BTreeSet, HashMap},
    fmt::{self, Write},
    io::{BufReader as StdBufReader, Seek},
    net::SocketAddr,
    os::unix::ffi::OsStrExt,
    path::{Path, PathBuf},
    str::FromStr,
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
    /// turn off the rather inefficent search page.
    /// probably a good idea if you want to expose wahs to the internet.
    #[argh(switch)]
    no_search: bool,
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

#[derive(Debug, Clone)]
struct WarcLocation {
    path: Arc<PathBuf>,
    offset: u64,
}

#[derive(Debug)]
struct AppState {
    directories: Vec<PathBuf>,
    cdx_map: RwLock<HashMap<String, BTreeMap<u64, WarcLocation>>>,
    log: RwLock<String>,
    search_enabled: bool,
}

impl AppState {
    async fn get_warc_response(
        &self,
        req_url: String,
        timestamp: u64,
    ) -> Result<WarcResponse, ResponseError> {
        let base_url = Url::parse(&req_url).map_err(ResponseError::UrlParse)?;
        let loc = {
            let cdx_map = self.cdx_map.read().await;
            let Some(ts_map) = cdx_map.get(&req_url) else {
                return Err(ResponseError::NotFound(req_url));
            };
            match ts_map.get(&timestamp) {
                Some(l) => l.clone(),
                None => {
                    if let Some((&newt, _)) = ts_map
                        .range(timestamp..)
                        .next()
                        .or_else(|| ts_map.range(..timestamp).next_back())
                    {
                        let loc = mangle_url(None, &req_url, newt).unwrap();
                        let mut headers = HeaderMap::new();
                        headers.insert(
                            "location",
                            HeaderValue::from_str(&loc).map_err(ResponseError::HeaderBorked)?,
                        );
                        return Ok(WarcResponse {
                            code: StatusCode::TEMPORARY_REDIRECT.into(),
                            headers,
                            body: vec![],
                        });
                    }
                    unreachable!();
                }
            }
        };
        let warc_path = loc.path.clone();

        let buffered = spawn_blocking(move || read_warc_record(&req_url, &loc, timestamp))
            .await
            .map_err(ResponseError::TokioJoin)??;

        let response = buffered.body();
        let mut headers = [httparse::EMPTY_HEADER; 256];
        let mut res = httparse::Response::new(&mut headers);
        let Ok(httparse::Status::Complete(body_offset)) = res.parse(response) else {
            return Err(ResponseError::HttpParse);
        };

        let mut headers = HeaderMap::new();
        for h in res.headers.iter() {
            if let Ok(k) = HeaderName::try_from(format!("x-archive-orig-{}", h.name)) {
                headers.insert(
                    k,
                    HeaderValue::from_bytes(h.value).map_err(ResponseError::HeaderBorked)?,
                );
            }
        }
        if let Ok(h) = HeaderValue::from_bytes(warc_path.as_os_str().as_bytes()) {
            headers.insert("x-archive-src", h);
        }
        headers.insert(
            "link",
            HeaderValue::from_str(&format!("<{base_url}>; rel=original"))
                .map_err(ResponseError::HeaderBorked)?,
        );
        let content_type = headers
            .get("x-archive-orig-content-type")
            .cloned()
            .unwrap_or(HeaderValue::from_static("text/html"));
        headers.insert("content-type", content_type);
        headers.insert(
            "cache-control",
            HeaderValue::from_static("public, max-age=604800, immutable"),
        );
        headers.insert(
            "content-security-policy",
            HeaderValue::from_static(
                "default-src 'self' 'unsafe-eval' 'unsafe-inline' data: blob:",
            ),
        );
        if let Some(location) = headers
            .get("x-archive-orig-location")
            .and_then(|h| h.to_str().ok())
            && let Some(mangled) = mangle_url(Some(&base_url), location, timestamp)
        {
            headers.insert(
                "location",
                HeaderValue::from_str(&mangled).map_err(ResponseError::HeaderBorked)?,
            );
        }
        let is_compressed = if let Some(encoding) = headers.get("x-archive-orig-content-encoding") {
            headers.insert("content-encoding", encoding.clone());
            true
        } else {
            false
        };
        let content_type = headers
            .get("content-type")
            .expect("we just added it lol")
            .to_str()
            .unwrap_or("application/octet-stream");
        let is_chunked = headers
            .get("x-archive-orig-transfer-encoding")
            // transfer-encoding is technically a list of directives,
            // but the (seldom used) other options are compression stuff
            // which we would not be able to handle anyways
            .is_some_and(|h| h == "chunked");

        let body = &response[body_offset..];

        let ct = content_type
            .split_once(';')
            .map_or(content_type, |(s, _)| s);
        // FIXME: treating xhtml like html is very naughty
        // people are usually nice enough to make their xhtml
        // html-compatible-ish tho
        let body = if !is_compressed
            && (ct.eq_ignore_ascii_case("text/html")
                || ct.eq_ignore_ascii_case("application/xhtml+xml"))
        {
            let mut output = vec![];
            let mut rewriter = HtmlRewriter::new(
                lol_html::Settings {
                    element_content_handlers: vec![
                        element!("[href]", |el| {
                            let Some(href) = el.get_attribute("href") else {
                                return Ok(());
                            };
                            let href = decode_html_entities(&href);
                            let Some(url) = mangle_url(Some(&base_url), &href, timestamp) else {
                                return Ok(());
                            };
                            let url = encode_double_quoted_attribute(&url);
                            _ = el.set_attribute("href", &url);
                            Ok(())
                        }),
                        element!("[src]", |el| {
                            let Some(src) = el.get_attribute("src") else {
                                return Ok(());
                            };
                            let src = decode_html_entities(&src);
                            let Some(url) = mangle_url(Some(&base_url), &src, timestamp) else {
                                return Ok(());
                            };
                            let url = encode_double_quoted_attribute(&url);
                            _ = el.set_attribute("src", &url);
                            Ok(())
                        }),
                        element!("[srcset]", |el| {
                            let Some(srcset) = el.get_attribute("srcset") else {
                                return Ok(());
                            };
                            let srcset: Vec<_> = srcset
                                .split_inclusive(',')
                                .map(str::trim_ascii)
                                .map(decode_html_entities)
                                .map(|s| match s.split_once(' ') {
                                    Some((s, r)) => format!(
                                        "{} {}",
                                        mangle_url(Some(&base_url), s, timestamp)
                                            .unwrap_or(Cow::Borrowed(s)),
                                        r
                                    ),
                                    None => mangle_url(Some(&base_url), &s, timestamp)
                                        .unwrap_or(Cow::Borrowed(&s))
                                        .to_string(),
                                })
                                .map(|s| encode_double_quoted_attribute(&s).to_string())
                                .collect();
                            _ = el.set_attribute("srcset", &srcset.join(" "));
                            Ok(())
                        }),
                    ],
                    ..lol_html::Settings::new()
                },
                |c: &[u8]| output.extend_from_slice(c),
            );

            if is_chunked {
                for chunk in UnChonk(body) {
                    rewriter.write(chunk).map_err(ResponseError::RewriteHtml)?;
                }
            } else {
                rewriter.write(body).map_err(ResponseError::RewriteHtml)?;
            }

            rewriter.end().map_err(ResponseError::RewriteHtml)?;
            output
        } else if is_chunked {
            let mut output = vec![];
            for chunk in UnChonk(body) {
                output.extend_from_slice(chunk);
            }
            output
        } else {
            body.to_vec()
        };

        Ok(WarcResponse {
            code: res.code.unwrap_or(200),
            headers,
            body,
        })
    }
}

fn mangle_url<'a>(base: Option<&Url>, join: &'a str, timestamp: u64) -> Option<Cow<'a, str>> {
    if join.starts_with("data:") {
        return Some(Cow::Borrowed(join));
    }
    let url = if let Some(base) = base {
        base.join(join).ok()
    } else {
        Url::parse(join).ok()
    }?;
    let url = url.as_str();
    let stop = url.find(['#', '?']).unwrap_or(url.len());
    let mut enc = utf8_percent_encode(&url[..stop], URL_UNSAFE).to_string();
    enc.push_str(&url[stop..]);
    let mut ptime = timestamp.to_string();
    // surely there is a better way to do this?
    ptime.truncate(ptime.trim_end_matches('0').len());
    if ptime.is_empty() {
        ptime.push('*');
    }
    Some(Cow::Owned(format!("/web/{ptime}/{enc}")))
}

fn demangle(inp: &str) -> Option<(u64, Url)> {
    let mut split = inp.split('/');
    if split.next().is_none_or(|s| !s.is_empty()) || split.next().is_none_or(|s| s != "web") {
        return None;
    }
    let timestamp = split.next().map(process_timestamp)?;
    let old: Vec<_> = split.collect();
    let old = old.join("/");
    let stop = old.find('?').unwrap_or(old.len());
    let mut url = percent_decode_str(&old[..stop])
        .decode_utf8()
        .ok()?
        .to_string();
    url.push_str(&old[stop..]);
    let url = Url::parse(&url).ok()?;
    Some((timestamp, url))
}

fn read_warc_record(
    req_url: &str,
    location: &WarcLocation,
    timestamp: u64,
) -> Result<warc::Record<warc::BufferedBody>, ResponseError> {
    let mut file = std::fs::File::open(location.path.as_ref())
        .map_err(|e| ResponseError::OpenWarc(e, format!("{:?}", location.path)))?;
    file.seek(std::io::SeekFrom::Start(location.offset))
        .map_err(|e| ResponseError::OpenWarc(e, format!("{:?}", location.path)))?;
    let mut file = WarcReader::new(StdBufReader::new(
        GzipReader::new(StdBufReader::with_capacity(1 << 20, file))
            .map_err(|e| ResponseError::OpenWarc(e, format!("{:?}", location.path)))?,
    ));
    let tstr = timestamp.to_string();
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
        if record.date().format("%Y%m%d%H%M%S").to_string() != tstr {
            continue;
        }
        return record
            .into_buffered()
            .map_err(|e| ResponseError::RecordBody(e, format!("{:?}", location.path)));
    }

    Err(ResponseError::WarcMissing(format!("{:?}", location.path)))
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
    NotFound(String),
    TokioJoin(tokio::task::JoinError),
    HttpParse,
    OpenWarc(std::io::Error, String),
    RecordBody(std::io::Error, String),
    WarcMissing(String),
    RewriteHtml(lol_html::errors::RewritingError),
    HeaderBorked(axum::http::header::InvalidHeaderValue),
}

impl fmt::Display for ResponseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(
            f,
            "<!DOCTYPE html>
<meta charset=UTF-8>
<meta name=viewport content=\"width=device-width, initial-scale=1\">
<title>error</title>"
        )?;
        match self {
            Self::UrlParse(parse_error) => write!(
                f,
                "<h1>could not parse provided url</h1><p>{}</p>",
                encode_text(&parse_error.to_string())
            ),
            Self::NotFound(url) => write!(
                f,
                "<h1>404 knot found</h1>
<p>the url you requested was not present in any of my cdx files.
<a href=\"{}\">maybe it's available on the web?</a></p>",
                encode_double_quoted_attribute(url)
            ),
            Self::TokioJoin(join_error) => write!(
                f,
                "<h1>what the tokio doin</h1><p>{}</p>",
                encode_text(&join_error.to_string())
            ),
            Self::HttpParse => write!(
                f,
                "<h1>could not parse http response in warc</h1>
            <p>... this warc file does have http stuff in it, right?</p>"
            ),
            Self::OpenWarc(error, path) => write!(
                f,
                "<h1>could knot open warc</h1>
<p>{}</p><p>if {} exists, i might have the wrong offset into it</p>",
                encode_text(&error.to_string()),
                encode_text(path)
            ),
            Self::RecordBody(error, path) => write!(
                f,
                "<h1>could not read warc record body</h1>
<p>{}</p><p>{} is probably corrupted</p>",
                encode_text(&error.to_string()),
                encode_text(path)
            ),
            Self::WarcMissing(path) => write!(
                f,
                "<h1>warc record missing</h1>
<p>a cdx file says it's in {} but i could not find it</p>",
                encode_text(path)
            ),
            Self::RewriteHtml(rewrite_error) => write!(
                f,
                "<h1>error rewriting html</h1><p>{}</p>",
                encode_text(&rewrite_error.to_string())
            ),
            Self::HeaderBorked(error) => write!(
                f,
                "<h1>invalid header value</h1><p>{}</p>",
                encode_text(&error.to_string())
            ),
        }
    }
}

#[derive(Debug)]
struct UnChonk<'a>(&'a [u8]);

impl<'a> Iterator for UnChonk<'a> {
    type Item = &'a [u8];
    fn next(&mut self) -> Option<Self::Item> {
        let mut len = 0;
        while let Some(digit) = self.0.first().and_then(|&b| unhex_ascii(b)) {
            len <<= 4;
            len += digit as usize;
            self.0 = &self.0[1..];
        }
        if len == 0 || self.0.len() < len + 4 {
            return None;
        }
        assert_eq!(&self.0[..2], b"\r\n");
        assert_eq!(&self.0[len + 2..len + 4], b"\r\n");
        let out = &self.0[2..len + 2];
        self.0 = &self.0[len + 4..];
        Some(out)
    }
}

fn unhex_ascii(b: u8) -> Option<u8> {
    match b {
        b'0'..=b'9' => Some(b - b'0'),
        b'A'..=b'F' => Some(b - b'A' + 0xa),
        b'a'..=b'f' => Some(b - b'a' + 0xa),
        _ => None,
    }
}

async fn root(State(state): State<Arc<AppState>>) -> Html<String> {
    let log = state.log.read().await;
    Html(format!(
        "<!DOCTYPE html>
<meta charset=UTF-8>
<meta name=viewport content=\"width=device-width, initial-scale=1\">
<title>wahs</title>
<h1>wahs</h1>
<p>go request stuff to <code>/web/YYYYMMDDHHMMSS/someurl</code>
(remember to url escape the url){}</p>
<h2>log</h2>
<pre>{}</pre>",
        if state.search_enabled {
            ", or <a href=search>search for a url</a>"
        } else {
            ""
        },
        encode_text(log.as_str())
    ))
}

fn process_timestamp(inp: &str) -> u64 {
    let mut numbers = inp.chars().take_while(|c| c.is_ascii_digit());
    let mut num = 0;
    for _ in 0..14 {
        num *= 10;
        num += numbers.next().map_or(0, |i| i as u64 - '0' as u64);
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
            let status = match e {
                ResponseError::UrlParse(_) => StatusCode::BAD_REQUEST,
                ResponseError::NotFound(_) => StatusCode::NOT_FOUND,
                _ => StatusCode::INTERNAL_SERVER_ERROR,
            };
            return (status, headers, e.to_string().into_bytes());
        }
    };

    (
        StatusCode::from_u16(response.code).unwrap_or(StatusCode::OK),
        response.headers,
        response.body,
    )
}

async fn search(
    State(state): State<Arc<AppState>>,
    Query(query): Query<BTreeMap<String, String>>,
) -> Html<String> {
    let query = query.get("q");
    let mut out = "<!DOCTYPE html>
<meta charset=UTF-8>
<meta name=viewport content=\"width=device-width, initial-scale=1\">
<title>wahs search</title><h1>search for urls</h1><form action=?>
<label>search <input name=q onfocus=this.select() "
        .to_string();
    if let Some(query) = query {
        write!(out, "value=\"{}\"", &encode_double_quoted_attribute(query)).unwrap();
    } else {
        out.push_str("autofocus");
    }
    out.push_str("></label> <input type=submit> </form>");

    if let Some(query) = query {
        let (mut neg, pos): (Vec<_>, Vec<_>) = query
            .split_ascii_whitespace()
            .partition(|w| w.starts_with("-"));
        neg.retain_mut(|w| {
            *w = &w[1..];
            !w.is_empty()
        });
        out.push_str("<h2>results</h2><ul>");
        let cdx_map = state.cdx_map.read().await;
        let mut results: Vec<_> = cdx_map
            .keys()
            .filter(|u| pos.iter().all(|w| u.contains(w)) && neg.iter().all(|w| !u.contains(w)))
            .take(1000)
            .collect();
        results.sort_by(|a, b| a.len().cmp(&b.len()).then_with(|| a.cmp(b)));
        for res in results {
            if let Some(mangled) = mangle_url(None, res, 0) {
                write!(
                    out,
                    "<li><a href=\"{}\">{}</a></li>",
                    encode_double_quoted_attribute(&mangled),
                    encode_text(res)
                )
                .unwrap();
            }
        }
        out.push_str("</ul>");
    }

    Html(out)
}

async fn fallback(
    State(state): State<Arc<AppState>>,
    uri: Uri,
    headers: HeaderMap,
) -> Result<(StatusCode, HeaderMap), StatusCode> {
    if let Some(refer) = headers.get("referer").and_then(|h| h.to_str().ok())
        && let Ok(refer) = Uri::from_str(refer)
        && let Some(refer) = refer.path_and_query().map(|p| p.as_str())
        && let Some((timestamp, base)) = demangle(refer)
        && let Some(join) = uri.path_and_query().map(|p| p.as_str())
        && let Ok(url) = base.join(join)
        && state.cdx_map.read().await.contains_key(url.as_str())
        && let Some(mangled) = mangle_url(None, url.as_str(), timestamp)
        && let Ok(loc) = HeaderValue::from_str(&mangled)
    {
        let mut headers = HeaderMap::new();
        headers.insert("location", loc);
        return Ok((StatusCode::TEMPORARY_REDIRECT, headers));
    }

    Err(StatusCode::NOT_FOUND)
}

async fn read_cdx(
    cdxname: &Path,
    map: &mut HashMap<String, BTreeMap<u64, WarcLocation>>,
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
    let offset = header.clone().find(|(_, c)| *c == "V").map(|(o, _)| o);
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
        let offset = offset
            .and_then(|o| columns.get(o))
            .and_then(|o| o.parse().ok())
            .unwrap_or(0);
        let warcname = parent.join(warcname);
        let warcname = if let Some(w) = dedup.get(&warcname) {
            w.clone()
        } else {
            let w = Arc::new(warcname);
            dedup.insert(w.clone());
            w
        };

        map.entry(url.to_string()).or_default().insert(
            date,
            WarcLocation {
                path: warcname,
                offset,
            },
        );
    }

    Ok(())
}

async fn reindex(
    dirs: &[PathBuf],
    old_size: usize,
) -> (HashMap<String, BTreeMap<u64, WarcLocation>>, String) {
    let mut dedup = BTreeSet::new();
    let mut map = HashMap::with_capacity(old_size);
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
        }

        writeln!(log, "indexed directory {dirname:?}").unwrap();
    }

    map.shrink_to_fit();
    write!(log, "finished indexing {} urls!", map.len()).unwrap();
    (map, log)
}

async fn reindex_forever(state: Arc<AppState>, interval: u64) {
    let interval = Duration::from_secs(interval);
    let mut old_size = 0;
    loop {
        let (map, stat) = reindex(&state.directories, old_size).await;
        old_size = map.len();
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
        cdx_map: RwLock::new(HashMap::new()),
        log: RwLock::new("not indexed yet...".to_string()),
        search_enabled: !opt.no_search,
    });

    tokio::task::spawn(reindex_forever(state.clone(), opt.interval));

    let app = Router::new().route("/", get(root));
    let app = if opt.no_search {
        app
    } else {
        app.route("/search", get(search))
    };
    let app = app
        .route("/web/{timestamp}/{*pathurl}", get(from_warc))
        .fallback(fallback)
        .with_state(state);

    let listen = TcpListener::bind(opt.bind).await.unwrap();
    eprintln!("listening on {}", listen.local_addr().unwrap());
    axum::serve(listen, app.into_make_service()).await.unwrap();
}
