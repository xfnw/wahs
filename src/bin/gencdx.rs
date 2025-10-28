use libflate::gzip::MultiDecoder as GzipReader;
use std::{
    borrow::Cow,
    env::args,
    io::{BufReader, Read},
    sync::atomic::{AtomicU64, Ordering},
};
use warc::{WarcHeader, WarcReader};

struct LengthReader<'a, T> {
    inner: T,
    count: &'a AtomicU64,
    start_location: &'a AtomicU64,
    expect_start: bool,
}

impl<T: Read> Read for LengthReader<'_, T> {
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        let res = self.inner.read(buf);
        if let Ok(size) = res {
            let count = self.count.fetch_add(size as u64, Ordering::Relaxed);
            let start_location = self.start_location.load(Ordering::Relaxed);
            if count <= start_location {
                self.expect_start = true;
            }
            if self.expect_start && size > 0 && count >= start_location {
                assert!(start_location == count, "start of gzip skipped over");
                assert!(buf.starts_with(&[31, 139]), "could not find start of gzip");
                self.expect_start = false;
            }
        }
        res
    }
}

fn main() {
    println!(" CDX a b m s k V g u");

    for filename in args().skip(1) {
        let mut old_offset;
        let mut offset = 0;
        let length = AtomicU64::new(0);
        let start_location = AtomicU64::new(0);
        let file = std::fs::File::open(&filename).expect("open warc file");
        // XXX: i expected the BufReader on the outside of GzipReader
        // to mess up our offsets, but it seems to be fine?
        // if it turns out that it does mess with it, can always just
        // set the capacity to 1 (to essentially lie about it being
        // buffered, since WarcReader requires BufRead), but that
        // causes a 3x performance penalty
        let file = WarcReader::new(BufReader::new(
            GzipReader::new(LengthReader {
                inner: BufReader::with_capacity(1 << 20, file),
                count: &length,
                start_location: &start_location,
                expect_start: true,
            })
            .expect("open gzip"),
        ));

        for record in file.iter_records() {
            old_offset = offset;
            offset = length.load(Ordering::Relaxed);
            start_location.store(offset + 8, Ordering::Relaxed);
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
            // XXX: 8 is the distance from where it stopped reading
            // the previous record and the start of this record for
            // all the warc files i tried, but all my warcs are from
            // wget, this might be relying on incorrect assumptions :(
            let v = old_offset + 8;
            // g - file name
            let g = &filename;
            // u - ??? missing from the cdx format reference, wget uses warc id
            let u = record.warc_id();

            println!("{a} {b} {m} {s} {k} {v} {g} {u}");
        }
    }
}
