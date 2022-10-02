#[derive(Debug, Clone, Copy)]
pub struct Index(pub usize);

#[derive(Debug, Clone, Copy)]
pub struct Level(pub usize);

impl Level {
    pub fn to_index(base: Level, current: Level) -> Index {
        Index(base.0 - current.0 - 1)
    }

    #[inline]
    pub fn inc(&self) -> Level {
        Level(self.0 + 1)
    }
}
