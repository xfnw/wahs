use rouille::Response;
use std::env::args;
use std::fs::File;
use std::io::BufRead;
use std::process::exit;
use warc::WarcHeader;
use warc::WarcReader;

fn rem_last(value: &str) -> &str {
    let mut chars = value.chars();
    chars.next_back();
    chars.as_str()
}

fn separate_content(request: &[u8]) -> (String, Vec<u8>) {
    const NL: &u8 = &0x0A;
    const CR: &u8 = &0x0D;
    const SP: &u8 = &0x20;
    const CL: &u8 = &0x3A;
    let mut prev: &u8 = &0x00;
    let mut anotherprev: &u8 = &0x00;
    let mut done = false;
    let mut newvec = Vec::new();
    for ch in request.iter() {
        if done {
            newvec.push(*ch)
        } else {
            if ch == NL && anotherprev == NL && prev == CR {
                done = true;
            }
            anotherprev = prev;
            prev = ch;
        }
    }

    let content_type = "text/html".to_string();
    (content_type, newvec as Vec<u8>)
}

fn search_warc<T: BufRead>(warc: WarcReader<T>, url: String) -> Result<(String, Vec<u8>), ()> {
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
                    let (content_type, body) = separate_content(record.body());
                    return Ok((content_type, body));
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
