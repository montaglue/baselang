pub trait Countable {
    fn from_count(count: usize) -> Self;
}

#[derive(Debug, Clone)]
pub struct Counter<T> {
    counter: usize,
    _marker: std::marker::PhantomData<T>,
}

impl<T: Countable> Counter<T> {
    pub fn new() -> Self {
        Counter {
            counter: 0,
            _marker: std::marker::PhantomData,
        }
    }

    pub fn next(&mut self) -> T {
        let counter = self.counter;
        self.counter += 1;
        <T as Countable>::from_count(counter)
    }
}
