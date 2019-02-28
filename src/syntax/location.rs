use memchr::Memchr;

#[derive(Copy, Clone, Debug)]
pub struct Location {
    row: u32,
    col: u32,
}

pub struct Source {
    lines: Box<[u32]>,
}

impl Source {
    pub fn new(input: &str) -> Self {
        let lines = Memchr::new(b'\n', input.as_bytes())
            .map(|p| p as u32)
            .collect();
        Source { lines }
    }

    pub fn location(&self, index: usize) -> Location {
        let index = index as u32;
        let row = match self.lines.binary_search(&index) {
            Ok(row) => row,
            Err(row) => row,
        } as u32;
        let col = index - row;
        Location { row, col }
    }
}
