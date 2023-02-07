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

fn chop_off_headers(value: &[u8]) -> Vec<u8> {
    let newline: &u8 = &0x0A;
    let carrage: &u8 = &0x0D;
    let mut prev: &u8 = &0x00;
    let mut anotherprev: &u8 = &0x00;
    let mut done = false;
    let mut newvec = Vec::new();
    for ch in value.iter() {
        if done {
            newvec.push(*ch)
        } else {
            if ch == newline && anotherprev == newline && prev == carrage {
                done = true;
            }
            anotherprev = prev;
            prev = ch;
        }
    }

    newvec as Vec<u8>
}

fn search_warc(
    warc: WarcReader<BufReader<MultiDecoder<BufReader<File>>>>,
    url: String,
) -> Result<(String, Vec<u8>), ()> {
    for record in warc.iter_records() {
        let record = match record {
            Ok(record) => record,
            Err(e) => {
                eprintln!("error: {}", e);
                continue;
            }
        };
        // FIXME: turns out WarcHeader::ContentType is just a weird warc type and not
        // the actual response's type :(
        // looks like i will have to parse the http headers instead of just
        // cutting them off and throwing them into the void.
        // also having the incorrect content type breaks css, for some reason
        match record.header(WarcHeader::WarcType) {
            Some(rtype) if rtype.eq("response") => match record.header(WarcHeader::TargetURI) {
                Some(h) if rem_last(&h).ends_with(&url) => {
                    //match &record.header(WarcHeader::ContentType) {
                    //    Some(ctype) => {
                    let body = chop_off_headers(record.body());
                    return Ok(("text/html".to_string(), body));
                    //        return Ok((ctype.to_string(), body));
                    //    }
                    //    None => eprintln!("error could not read ContentType"),
                    //}
                }
                _ => (),
            },
            _ => (),
        };
    }
    Err(())
}

fn main() {
    let filename = match args().nth(1) {
        Some(h) => h,
        None => {
            eprintln!("usage: {} file", args().next().expect("bap."));
            exit(1);
        }
    };

    rouille::start_server("localhost:8000", move |request| {
        // FIXME: very slow :(
        // maybe read earlier and then access with an Arc?
        let file = WarcReader::from_path_gzip(&filename).expect("failed to read warc file");

        println!("{:?}", request.url());
        {
            if request.url() == "/hello" {
                return Response::text("hello world");
            }
        }
        match search_warc(file, request.url()) {
            Ok((ctype, body)) => Response::from_data(ctype, body),
            Err(_) => Response::html(
                "<h1>error: 404 not found</h1>\n\
                    the page you requested is not part of this warc file.",
            ),
        }
    });
}
