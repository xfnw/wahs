use argh::FromArgs;
use axum::{
    extract::{Path, RawQuery},
    response::Html,
    response::IntoResponse,
    routing::get,
    Router,
};
use std::{net::SocketAddr, path::PathBuf};
use tokio::net::TcpListener;

/// serve warc files on http
#[derive(Debug, FromArgs)]
#[argh(help_triggers("-h", "--help"))]
struct Opt {
    /// socket address to bind
    #[argh(option, short = 'b', default = "\"[::1]:0\".parse().unwrap()")]
    bind: SocketAddr,
    /// path(s) to directory of warc and cdx files
    #[argh(positional)]
    directory: Vec<PathBuf>,
}

async fn root() -> Html<&'static str> {
    Html("wah")
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
    Path((timestamp, path_url)): Path<(String, String)>,
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

#[tokio::main]
async fn main() {
    let opt: Opt = argh::from_env();

    let app = Router::new()
        .route("/", get(root))
        .route("/{timestamp}/{*pathurl}", get(from_warc));

    let listen = TcpListener::bind(opt.bind).await.unwrap();
    eprintln!("listening on {}", listen.local_addr().unwrap());
    axum::serve(listen, app.into_make_service()).await.unwrap();
}
