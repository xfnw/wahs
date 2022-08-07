extern crate rouille;
use rouille::Response;

extern crate warc;
use warc::WarcHeader;
use warc::WarcReader;

extern crate libflate;
use std::env::args;
use std::process::exit;
use std::fs::File;
use std::io::BufReader;
use libflate::gzip::MultiDecoder;

fn search_warc(warc: WarcReader<BufReader<MultiDecoder<BufReader<File>>>>, url: &str) {
    for record in warc.iter_records() {
        match record {
            Ok(record) => {},
            Err(e) => eprintln!("error: {}",e)
        }
    }
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

    search_warc(file, "meow");

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
