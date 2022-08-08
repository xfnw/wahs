extern crate rouille;
use rouille::Response;

extern crate warc;
use warc::WarcHeader;
use warc::WarcReader;

extern crate libflate;
use libflate::gzip::MultiDecoder;
use std::env::args;
use std::fs::File;
use std::io::BufReader;
use std::process::exit;

fn rem_last(value: &str) -> &str {
    let mut chars = value.chars();
    chars.next_back();
    chars.as_str()
}

fn search_warc(
    warc: WarcReader<BufReader<MultiDecoder<BufReader<File>>>>,
    url: &str,
) -> Result<String, ()> {
    for record in warc.iter_records() {
        let record = match record {
            Ok(record) => record,
            Err(e) => {
                eprintln!("error: {}", e);
                continue;
            }
        };
        match record.header(WarcHeader::TargetURI) {
            Some(h) if rem_last(&h).ends_with(&url) => {
                let body = match std::str::from_utf8(&record.body()) {
                    Ok(h) => Ok(h.to_string()),
                    Err(_) => continue,
                };
                return body;
            }
            _ => (),
        };
    }
    return Err(());
}

fn main() {
    let filename = &match args().nth(1) {
        Some(h) => h,
        None => {
            eprintln!("usage: {} file", args().nth(0).expect("bap."));
            exit(1);
        }
    };

    let file = WarcReader::from_path_gzip(filename).expect("failed to read warc file");

    search_warc(file, "png");

    rouille::start_server("localhost:8000", move |request| {
        println!("{:?}", request.url());
        {
            if request.url() == "/hello" {
                return Response::text("hello world");
            }
        }
        return Response::html(
            "<h1>error: 404 not found</h1>\n\
                the page you requested is not part of this warc file.",
        );
    });
}
