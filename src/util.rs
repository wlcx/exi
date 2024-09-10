use std::ops::{Bound, RangeBounds};

use nom::{bits::complete::take, IResult};

// Alias for convenience
pub type BitInput<'a> = (&'a [u8], usize);

pub fn ilog2_ceil(v: usize) -> u32 {
    usize::BITS - (v - 1).leading_zeros()
}

// Return the number of values in a closed bound, with None meaning the bound open
pub fn bound_values<B>(b: &B) -> Option<u64>
where
    B: RangeBounds<i64>,
{
    match (b.start_bound(), b.end_bound()) {
        (Bound::Included(s), Bound::Included(e)) => Some(e.abs_diff(*s) + 1),
        (Bound::Included(s), Bound::Excluded(e)) => Some(e.abs_diff(*s)),
        (Bound::Excluded(s), Bound::Included(e)) => Some(e.abs_diff(*s)),
        (Bound::Excluded(s), Bound::Excluded(e)) => Some(e.abs_diff(*s) - 1),
        (_, _) => None,
    }
}

pub fn lower_bound<B>(b: &B) -> Option<i64>
where
    B: RangeBounds<i64>,
{
    match b.start_bound() {
        Bound::Included(s) => Some(*s),
        Bound::Excluded(s) => Some(s + 1),
        Bound::Unbounded => None,
    }
}

// Parse the remaining bits left in an input to make it byte-aligned again
pub fn trailing_bits(i: BitInput) -> IResult<BitInput, usize> {
    take(8 - i.1)(i)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_bound_values() {
        assert_eq!(bound_values(&..), None);
        assert_eq!(bound_values(&..5), None);
        assert_eq!(bound_values(&..=-1), None);
        assert_eq!(bound_values(&(-3..)), None);

        assert_eq!(bound_values(&(1..1)), Some(0));
        assert_eq!(bound_values(&(1..2)), Some(1));
        assert_eq!(bound_values(&(1..=2)), Some(2));
        assert_eq!(bound_values(&(2..=2)), Some(1));

        assert_eq!(bound_values(&(10..9)), Some(1));
        assert_eq!(bound_values(&(10..=9)), Some(2));
        assert_eq!(bound_values(&(1..=-1)), Some(3));
    }

    #[test]
    fn test_lower_bound() {
        assert_eq!(lower_bound(&..-6), None);
        assert_eq!(lower_bound(&(1..)), Some(1));
        assert_eq!(lower_bound(&(-5..-1)), Some(-5));

        assert_eq!(lower_bound(&(-1..-5)), Some(-1));
    }
}
