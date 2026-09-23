use std::fmt;

pub struct NonEmptyVec<T> {
    first: T,
    rest: Vec<T>,
}

impl<T> fmt::Debug for NonEmptyVec<T>
where
    T: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("NonEmptyVec")
            .field("first", &self.first)
            .field("rest", &self.rest)
            .finish()
    }
}

impl<T> PartialEq for NonEmptyVec<T>
where
    T: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        self.first == other.first && self.rest == other.rest
    }
}

impl<T> Eq for NonEmptyVec<T> where T: Eq {}

impl<T> NonEmptyVec<T> {
    pub fn new(mut from: Vec<T>) -> Self {
        assert!(!from.is_empty());
        let first = from.remove(0);
        Self { first, rest: from }
    }

    pub fn len(&self) -> usize {
        self.rest.len() + 1
    }

    pub fn first(&self) -> &T {
        &self.first
    }

    pub fn get(&self, index: usize) -> Option<&T> {
        match index {
            0 => Some(&self.first),
            i => self.rest.get(i - 1),
        }
    }

    pub fn iter<'a>(&'a self) -> IntoIterRef<'a, T> {
        self.into_iter()
    }
}

pub struct IntoIter<T> {
    first: Option<T>,
    rest: std::vec::IntoIter<T>,
}

impl<T> Iterator for IntoIter<T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        match self.first.take() {
            Some(first) => Some(first),
            None => self.rest.next(),
        }
    }
}

pub struct IntoIterRef<'a, T> {
    first: Option<&'a T>,
    rest: std::slice::Iter<'a, T>,
}

impl<'a, T> Iterator for IntoIterRef<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        match self.first.take() {
            Some(first) => Some(first),
            None => self.rest.next(),
        }
    }
}

impl<T> IntoIterator for NonEmptyVec<T> {
    type Item = T;

    type IntoIter = IntoIter<T>;

    fn into_iter(self) -> Self::IntoIter {
        IntoIter {
            first: Some(self.first),
            rest: self.rest.into_iter(),
        }
    }
}

impl<'a, T> IntoIterator for &'a NonEmptyVec<T> {
    type Item = &'a T;

    type IntoIter = IntoIterRef<'a, T>;

    fn into_iter(self) -> IntoIterRef<'a, T> {
        IntoIterRef {
            first: Some(&self.first),
            rest: self.rest.iter(),
        }
    }
}
