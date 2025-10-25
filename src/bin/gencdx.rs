use std::{borrow::Cow, env::args};
use warc::{WarcHeader, WarcReader};

fn main() {
    // would be nice to include `V` (compressed offset) too, but the warc crate does not give us
    // that information :(
    println!(" CDX a b m s k g u");

    for filename in args().skip(1) {
        let file = WarcReader::from_path_gzip(&filename).expect("failed to read warc file");

        for record in file.iter_records() {
            let Ok(record) = record else {
                continue;
            };
            if record
                .header(WarcHeader::WarcType)
                .is_none_or(|h| h != "response")
            {
                continue;
            }
            let mut headers = [httparse::EMPTY_HEADER; 256];
            let mut res = httparse::Response::new(&mut headers);
            if res.parse(record.body()).is_err() {
                continue;
            }

            // a - original url
            let Some(a) = record.header(WarcHeader::TargetURI) else {
                continue;
            };
            let a = a.strip_prefix("<").unwrap_or(&a);
            let a = a.strip_suffix(">").unwrap_or(a);
            // b - date
            let b = record.date().format("%Y%m%d%H%M%S");
            // m - mime type of original document
            let m = res
                .headers
                .iter()
                .find(|h| h.name.eq_ignore_ascii_case("content-type"))
                .map(|h| h.value.to_vec())
                .and_then(|v| String::from_utf8(v).ok())
                .map(Cow::Owned)
                .unwrap_or(Cow::Borrowed("-"));
            let m = m.split_once([';', ' ']).map(|(m, _)| m).unwrap_or(&m);
            // s - response code
            let s = res
                .code
                .map(|n| Cow::Owned(n.to_string()))
                .unwrap_or(Cow::Borrowed("-"));
            // k - new style checksum
            let k = record
                .header(WarcHeader::PayloadDigest)
                .unwrap_or(Cow::Borrowed("-"));
            let k = k.split_once(":").map(|(_, k)| k).unwrap_or(&k);
            // g - file name
            let g = &filename;
            // u - ??? missing from the cdx format reference, wget uses warc id
            let u = record.warc_id();

            println!("{a} {b} {m} {s} {k} {g} {u}");
        }
    }
}
