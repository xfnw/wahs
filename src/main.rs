use argh::FromArgs;
use axum::{response::Html, routing::get, Router};
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

#[tokio::main]
async fn main() {
    let opt: Opt = argh::from_env();

    let app = Router::new().route("/", get(root));

    let listen = TcpListener::bind(opt.bind).await.unwrap();
    eprintln!("listening on {}", listen.local_addr().unwrap());
    axum::serve(listen, app.into_make_service()).await.unwrap();
}
