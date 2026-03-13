use std::{
    cmp::min,
    io::{Read, Result},
};

pub enum ExtractLayer<'a> {
    Slice(&'a [u8]),
    Chunked(ChunkedExtract<'a>),
}

impl<'a> ExtractLayer<'a> {
    pub fn add_layers<'b, T>(self, mut layers: T) -> Option<Self>
    where
        T: Iterator<Item = &'b [u8]>,
    {
        let Some(next) = layers.next() else {
            return Some(self);
        };
        match next {
            b"chunked" => Self::Chunked(ChunkedExtract::new(self)),
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
        }
    }
}

pub struct ChunkedExtract<'a> {
    inner: Box<ExtractLayer<'a>>,
    state: ChunkedState,
}

impl<'a> ChunkedExtract<'a> {
    pub fn new(inner: ExtractLayer<'a>) -> Self {
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
