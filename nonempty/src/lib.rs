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

impl<T> Clone for NonEmptyVec<T>
where
    T: Clone,
{
    fn clone(&self) -> Self {
        Self {
            first: self.first.clone(),
            rest: self.rest.clone(),
        }
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
    pub fn new(mut from: Vec<T>) -> Option<Self> {
        if from.is_empty() {
            return None;
        }
        let first = from.remove(0);
        Some(Self { first, rest: from })
    }

    #[expect(
        clippy::len_without_is_empty,
        reason = "yeah clippy, but `empty()` does not *really* make sense here ;)"
    )]
    pub fn len(&self) -> usize {
        self.rest.len().checked_add(1).expect("vec at capacity")
    }

    pub fn first(&self) -> &T {
        &self.first
    }

    pub fn get(&self, index: usize) -> Option<&T> {
        match index {
            0 => Some(&self.first),
            i => self
                .rest
                .get(i.checked_sub(1).expect("checked for index 0 above")),
        }
    }

    pub fn iter(&self) -> IntoIterRef<'_, T> {
        self.into_iter()
    }

    pub fn into_vec(self) -> Vec<T> {
        self.into_iter().collect()
    }
}

impl<T> NonEmptyVec<T>
where
    T: Clone,
{
    pub fn sort_by_key<F, K>(&mut self, f: F)
    where
        F: Fn(&T) -> K,
        K: Ord,
    {
        let mut sorted = {
            let mut vec = self.clone().into_vec();
            vec.sort_by_key(f);
            vec
        };

        let rest = sorted.split_off(1);
        let first = sorted.pop().expect("leftover is exactly 1");

        self.first = first;
        self.rest = rest;
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

#[cfg(test)]
#[expect(
    clippy::default_numeric_fallback,
    reason = "readability, and the type is irrelevant"
)]
mod tests {
    use tap::Tap as _;

    use super::*;

    #[test]
    fn test_from_empty() {
        assert!(NonEmptyVec::<bool>::new(vec![]).is_none());
    }

    #[test]
    fn test_sorting() {
        assert_eq!(
            NonEmptyVec::new(vec![1, 2, 3, 4])
                .unwrap()
                .tap_mut(|vec| vec.sort_by_key(|i| *i)),
            NonEmptyVec::new(vec![1, 2, 3, 4]).unwrap()
        );
        assert_eq!(
            NonEmptyVec::new(vec![1, 2, 3, 4])
                .unwrap()
                .tap_mut(|vec| vec.sort_by_key(|i| -*i)),
            NonEmptyVec::new(vec![4, 3, 2, 1]).unwrap()
        );
        assert_eq!(
            NonEmptyVec::new(vec![615, 748, 372, 621, 920])
                .unwrap()
                .tap_mut(|vec| vec.sort_by_key(|i| *i)),
            NonEmptyVec::new(vec![372, 615, 621, 748, 920]).unwrap()
        );
    }
}
