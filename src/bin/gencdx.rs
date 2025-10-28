use libflate::gzip::Decoder as GzipDecoder;
use std::{
    borrow::Cow,
    env::args,
    io::{BufReader, Read},
    sync::atomic::{AtomicU64, Ordering},
};
use warc::{WarcHeader, WarcReader};

struct GzipReader<'a, T> {
    inner: Option<GzipDecoder<LengthReader<'a, T>>>,
    last_member: &'a AtomicU64,
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
            let start = inner.as_inner_ref().count.load(Ordering::Relaxed);
            let mut inner = match GzipDecoder::new(self.inner.take().unwrap().into_inner()) {
                Ok(i) => i,
                Err(e) => {
                    return Err(e);
                }
            };
            self.last_member.store(start, Ordering::Relaxed);
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
    count: &'a AtomicU64,
}

impl<T: Read> Read for LengthReader<'_, T> {
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        let res = self.inner.read(buf);
        if let Ok(size) = res {
            self.count.fetch_add(size as u64, Ordering::Relaxed);
        }
        res
    }
}

fn main() {
    println!(" CDX a b m s k V g u");

    for filename in args().skip(1) {
        let last_member = AtomicU64::new(0);
        let length = AtomicU64::new(0);
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
            // V - file offset (before decompression)
            // should be uppercase, but rust is anal about naming
            let v = last_member.load(Ordering::Relaxed);
            // g - file name
            let g = &filename;
            // u - ??? missing from the cdx format reference, wget uses warc id
            let u = record.warc_id();

            println!("{a} {b} {m} {s} {k} {v} {g} {u}");
        }
    }
}
