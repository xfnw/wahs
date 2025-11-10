use libflate::gzip::Decoder as GzipDecoder;
use std::{
    borrow::Cow,
    cell::Cell,
    env::args,
    io::{BufReader, Read},
};
use warc::{WarcHeader, WarcReader};

struct GzipReader<'a, T> {
    inner: Option<GzipDecoder<LengthReader<'a, T>>>,
    last_member: &'a Cell<u64>,
}

// adapted from libflate's MultiDecoder Read implementation
// but less good because libflate makes all the fun stuff private >:(
impl<T: Read> Read for GzipReader<'_, T> {
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        let Some(ref mut inner) = self.inner else {
            return Ok(0);
        };
        let size = inner.read(buf)?;
        if size == 0 {
            let start = inner.as_inner_ref().count.get();
            let mut inner = match GzipDecoder::new(self.inner.take().unwrap().into_inner()) {
                Ok(i) => i,
                Err(e) => {
                    return Err(e);
                }
            };
            self.last_member.set(start);
            let newsize = inner.read(buf);
            self.inner = Some(inner);
            newsize
        } else {
            Ok(size)
        }
    }
}

struct LengthReader<'a, T> {
    inner: T,
    count: &'a Cell<u64>,
}

impl<T: Read> Read for LengthReader<'_, T> {
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        let res = self.inner.read(buf);
        if let Ok(size) = res {
            self.count.update(|x| x + size as u64);
        }
        res
    }
}

fn main() {
    println!(" CDX a b m s k V g u");

    for filename in args().skip(1) {
        let last_member = Cell::new(0);
        let length = Cell::new(0);
        let file = std::fs::File::open(&filename).expect("open warc file");
        let file = WarcReader::new(BufReader::new(GzipReader {
            inner: Some(
                GzipDecoder::new(LengthReader {
                    inner: BufReader::with_capacity(1 << 20, file),
                    count: &length,
                })
                .expect("open gzip"),
            ),
            last_member: &last_member,
        }));

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
                .map_or(Cow::Borrowed("-"), Cow::Owned);
            let m = m.split_once([';', ' ']).map_or(m.as_ref(), |(m, _)| m);
            // s - response code
            let s = res
                .code
                .map_or(Cow::Borrowed("-"), |n| Cow::Owned(n.to_string()));
            // k - new style checksum
            let k = record
                .header(WarcHeader::PayloadDigest)
                .unwrap_or(Cow::Borrowed("-"));
            let k = k.split_once(':').map_or(k.as_ref(), |(_, k)| k);
            // V - file offset (before decompression)
            // should be uppercase, but rust is anal about naming
            let v = last_member.get();
            // g - file name
            let g = &filename;
            // u - ??? missing from the cdx format reference, wget uses warc id
            let u = record.warc_id();

            println!("{a} {b} {m} {s} {k} {v} {g} {u}");
        }
    }
}
