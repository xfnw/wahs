use rouille::Response;
use rouille::ResponseBody;
use std::env::args;
use std::io::BufRead;
use std::process::exit;
use warc::WarcHeader;
use warc::WarcReader;

enum WarcPage {
    Found(String, Vec<u8>),
    NoType(Vec<u8>),
    ParseError,
    NotFound,
}

fn rem_last(value: &str) -> &str {
    let mut chars = value.chars();
    chars.next_back();
    chars.as_str()
}

fn separate_content(request: &[u8]) -> Option<(Option<String>, Vec<u8>)> {
    let mut headers = [httparse::EMPTY_HEADER; 256];
    let mut res = httparse::Response::new(&mut headers);
    let Ok(httparse::Status::Complete(boffset)) = res.parse(request) else {
        return None;
    };
    let content_type = headers
        .iter()
        .find(|h| h.name.eq_ignore_ascii_case("content-type"))
        .map(|h| h.value.to_vec())
        .and_then(|v| String::from_utf8(v).ok());
    let body = request[boffset..].to_vec();

    Some((content_type, body))
}

fn search_warc<T: BufRead>(warc: WarcReader<T>, url: String) -> WarcPage {
    for record in warc.iter_records() {
        let record = match record {
            Ok(record) => record,
            Err(e) => {
                eprintln!("error: {}", e);
                continue;
            }
        };

        match record.header(WarcHeader::WarcType) {
            Some(rtype) if rtype.eq("response") => match record.header(WarcHeader::TargetURI) {
                Some(h) if rem_last(&h).ends_with(&url) => {
                    let Some((content_type, body)) = separate_content(record.body()) else {
                        return WarcPage::ParseError;
                    };
                    let Some(content_type) = content_type else {
                        return WarcPage::NoType(body);
                    };
                    return WarcPage::Found(content_type, body);
                }
                _ => (),
            },
            _ => (),
        };
    }
    WarcPage::NotFound
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
        let file = WarcReader::from_path_gzip(&filename).expect("failed to read warc file");

        println!("{:?}", request.url());
        match search_warc(file, request.url()) {
            WarcPage::Found(ctype, body) => Response::from_data(ctype, body),
            WarcPage::NoType(body) => Response {
                status_code: 200,
                headers: vec![],
                data: ResponseBody::from_data(body),
                upgrade: None,
            },
            WarcPage::NotFound => Response {
                status_code: 404,
                headers: vec![("Content-Type".into(), "text/html".into())],
                data: ResponseBody::from_data(
                    "<h1>404 not found</h1>\nthe file you requested is not part of this warc file.",
                ),
                upgrade: None,
            },
            WarcPage::ParseError => Response {
                status_code: 500,
                headers: vec![("Content-Type".into(), "text/html".into())],
                data: ResponseBody::from_data(
                    "<h1>500 internal server error</h1>\ntheres probably too many headers on this page.",
                ),
                upgrade: None,
            },
        }
    });
}
