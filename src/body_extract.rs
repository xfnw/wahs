use axum::http::HeaderValue;
use libflate::gzip::MultiDecoder;
use std::{
    cmp::min,
    io::{Read, Result},
};

pub struct BodyExtract<'a> {
    inner: ExtractLayer<'a>,
}

impl<'a> BodyExtract<'a> {
    pub fn new(
        body: &'a [u8],
        transfer_encoding: Option<&HeaderValue>,
        content_encoding: Option<&HeaderValue>,
    ) -> Option<Self> {
        // squishing the transfer encoding and content encoding
        // together like this is incorrect, they have different
        // allowed values, i doubt it matters though
        let layers = content_encoding
            .iter()
            .chain(transfer_encoding.iter())
            .map(|h| h.as_bytes())
            .flat_map(|h| h.split(|&b| b == b','))
            .map(|e| e.trim_ascii())
            .rev();
        ExtractLayer::Slice(body)
            .add_layers(layers)
            .map(|inner| Self { inner })
    }
}

impl<'a> Read for BodyExtract<'a> {
    fn read(&mut self, buf: &mut [u8]) -> Result<usize> {
        self.inner.read(buf)
    }
}

enum ExtractLayer<'a> {
    Slice(&'a [u8]),
    Chunked(ChunkedExtract<'a>),
    Gzip(GzipExtract<'a>),
    // TODO: support zstd and brotli too?
}

impl<'a> ExtractLayer<'a> {
    fn add_layers<'b, T>(self, mut layers: T) -> Option<Self>
    where
        T: Iterator<Item = &'b [u8]>,
    {
        let Some(next) = layers.next() else {
            return Some(self);
        };
        match next {
            b"chunked" => Self::Chunked(ChunkedExtract::new(self)),
            b"gzip" => Self::Gzip(GzipExtract::new(self)?),
            // a content-encoding of "none" is not a thing that exists:
            // https://www.iana.org/assignments/http-parameters/http-parameters.xhtml#content-coding
            // despite this, some sites still set it...
            b"none" => self,
            _ => return None,
        }
        .add_layers(layers)
    }
}

impl Read for ExtractLayer<'_> {
    fn read(&mut self, buf: &mut [u8]) -> Result<usize> {
        match self {
            Self::Slice(l) => l.read(buf),
            Self::Chunked(l) => l.read(buf),
            Self::Gzip(l) => l.read(buf),
        }
    }
}

struct ChunkedExtract<'a> {
    inner: Box<ExtractLayer<'a>>,
    state: ChunkedState,
}

impl<'a> ChunkedExtract<'a> {
    fn new(inner: ExtractLayer<'a>) -> Self {
        Self {
            inner: Box::new(inner),
            state: ChunkedState::Number { value: 0 },
        }
    }
}

impl Read for ChunkedExtract<'_> {
    fn read(&mut self, buf: &mut [u8]) -> Result<usize> {
        match self.state {
            ChunkedState::Content { remaining } => {
                if remaining == 0 {
                    expect_eat(&mut self.inner, b'\r')?;
                    expect_eat(&mut self.inner, b'\n')?;
                    self.state = ChunkedState::Number { value: 0 };
                    return self.read(buf);
                }
                let available = min(remaining, buf.len());
                let count = self.inner.read(&mut buf[..available])?;
                self.state = ChunkedState::Content {
                    remaining: remaining - count,
                };
                Ok(count)
            }
            ChunkedState::Number { value } => {
                let mut nbuf = [0u8; 1];
                self.inner.read_exact(&mut nbuf)?;

                let digit = match nbuf[0] {
                    b'\r' => {
                        expect_eat(&mut self.inner, b'\n')?;
                        self.state = if value == 0 {
                            ChunkedState::Done
                        } else {
                            ChunkedState::Content { remaining: value }
                        };
                        return self.read(buf);
                    }
                    n @ b'0'..=b'9' => n - b'0',
                    n @ b'a'..=b'f' => n - b'a' + 0xa,
                    n @ b'A'..=b'F' => n - b'A' + 0xa,
                    c => {
                        return Err(std::io::Error::other(format!(
                            "unexpected character {c:02x} when looking for chunk length digit"
                        )));
                    }
                };

                self.state = ChunkedState::Number {
                    value: value * 0x10 + digit as usize,
                };
                self.read(buf)
            }
            ChunkedState::Done => Ok(0),
        }
    }
}

enum ChunkedState {
    Content { remaining: usize },
    Number { value: usize },
    Done,
}

fn expect_eat(inp: &mut impl Read, expected: u8) -> Result<()> {
    let mut buf = [0u8; 1];
    inp.read_exact(&mut buf)?;

    if buf[0] != expected {
        return Err(std::io::Error::other(format!(
            "expected {:02x} got {:02x}",
            expected, buf[0]
        )));
    }

    Ok(())
}

struct GzipExtract<'a> {
    inner: MultiDecoder<Box<ExtractLayer<'a>>>,
}

impl<'a> GzipExtract<'a> {
    fn new(inner: ExtractLayer<'a>) -> Option<Self> {
        Some(Self {
            inner: MultiDecoder::new(Box::new(inner)).ok()?,
        })
    }
}

impl Read for GzipExtract<'_> {
    fn read(&mut self, buf: &mut [u8]) -> Result<usize> {
        self.inner.read(buf)
    }
}
