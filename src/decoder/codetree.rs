use std::{cmp::Ordering, iter::once, path::Iter};

use nom::IResult;
use nom::bits::complete::take;

use crate::util::{BitInput, ilog2_ceil};

// Describes the tree of event codes in an EXI grammar.
// `CodeTree<T>`s always:
// - Consist of 1 or more levels
// - Each level having 0 or more left `T`s
// - And have exactly one right `T`, which is either another CodeTree, or a terminal `T``
#[derive(Clone, PartialEq, Debug)]
pub(super) enum CodeTree<T: Clone> {
    Terminal(T),
    Node {
        left: Vec<T>,
        right: Box<CodeTree<T>>,
    },
}

struct CodeTree2<T> {
    first: T,
    more: Vec<T>,
    last: Option<Box<CodeTree2<T>>>,
}

impl<T: Clone> CodeTree<T> {
    pub(super) fn map<F, U: Clone>(self, f: F) -> CodeTree<U>
    where
        F: Fn(T) -> U,
    {
        match self {
            Self::Terminal(v) => CodeTree::Terminal(f(v)),
            Self::Node { left, right } => CodeTree::Node {
                left: left.into_iter().map(&f).collect(),
                right: right.map(f).into(),
            },
        }
    }
}

impl<T: Clone + std::fmt::Debug> CodeTree<T> {
    // Convenience function to build a codetree from a series of vecs, with each vec
    // containing elements to add at a "layer" of the codetree.
    pub(super) fn from_vecs(mut vecs: Vec<Vec<T>>) -> Self {
        while let Some(v) = vecs.last()
            && v.is_empty()
        {
            vecs.pop();
        }
        let this = vecs.remove(0);
        let islast = vecs.is_empty();
        match (this.len(), islast) {
            (_, false) => Self::Node {
                left: this,
                right: Self::from_vecs(vecs).into(),
            },
            (0, true) => {
                panic!("Trailing 0-length codetree layers are invalid")
            }
            (1, true) => Self::Node {
                left: vec![],
                right: Self::Terminal(this[0].to_owned()).into(),
            },
            (l @ 2.., true) => Self::Node {
                left: this[0..l - 1].to_owned(),
                right: Self::Terminal(this[l - 1].to_owned()).into(),
            },
        }
    }
}

impl<T: Clone + std::fmt::Display> CodeTree<T> {
    fn _pprint(&self, prefix: &str) -> Vec<(String, String)> {
        let mut out = vec![];
        match self {
            Self::Node { left, right } => {
                out.extend(
                    left.iter()
                        .enumerate()
                        .map(|(i, t)| (format!("{}{}", prefix, i), format!("{}", t))),
                );
                match right.as_ref() {
                    Self::Node { left: _, right: _ } => {
                        out.append(&mut right._pprint(&format!("{}{}.", prefix, left.len())));
                    }
                    Self::Terminal(t) => {
                        out.push((format!("{}{}", prefix, left.len()), format!("{}", t)));
                    }
                }
            }
            Self::Terminal(t) => out.push((format!("{}0", prefix), format!("{}", t))),
        }
        out
    }

    pub(super) fn pprint(&self) -> String {
        self._pprint("")
            .iter()
            .map(|(a, b)| format!("{}\t\t{}", a, b))
            .collect::<Vec<_>>()
            .join("\n")
    }
}

impl<'a, T: Clone + 'a> CodeTree<T> {
    pub(super) fn len(&self) -> Vec<usize> {
        match self {
            Self::Terminal(_) => {
                vec![1]
            }
            Self::Node { left, right } => {
                let slen = left.len() + 1;
                if let Self::Terminal(_) = right.as_ref() {
                    vec![slen]
                } else {
                    let mut v = right.len();
                    v.insert(0, slen);
                    v
                }
            }
        }
    }

    // The minimum number of bits needed to represent each component of the code
    // For a code with lengths (2, 3) this would be (1, 2). If the code has just one
    // terminal, thus having a length (1,), the bits required would be (0,) - a single
    // possiblity is encoded in 0 bits.
    pub(super) fn bits(&self) -> Vec<u32> {
        self.len().into_iter().map(ilog2_ceil).collect()
    }

    // Return a copy of the codetree with element `v` inserted at `index`.
    //
    // N.B that this only works at the "top" level. You can't currently insert into "lower" levels.
    pub(super) fn insert(&self, index: usize, v: T) -> Self {
        match (self, index) {
            // Turn a terminal into a node, adding to the left
            (CodeTree::Terminal(i), 0) => CodeTree::Node {
                left: vec![v],
                right: CodeTree::Terminal(i.clone()).into(),
            },
            // Turn a terminal into a node, adding to the right
            (CodeTree::Terminal(i), 1) => CodeTree::Node {
                left: vec![i.clone()],
                right: CodeTree::Terminal(v).into(),
            },
            // Given a terminal is just one node, any other insert index is invalid
            (CodeTree::Terminal(_), _) => panic!("Can't insert"),
            // Modify a node, inserting somewhere in the left hand side
            (CodeTree::Node { left, right }, idx) => {
                if idx > left.len() {
                    panic!("can't insert");
                }
                let mut new_left = left.clone();
                new_left.insert(idx, v);
                CodeTree::Node {
                    left: new_left,
                    right: right.clone(),
                }
            }
        }
    }

    // Given this codetree, produce the next event from bitstream i
    pub(super) fn parse<'b>(&self, i: BitInput<'b>) -> IResult<BitInput<'b>, T> {
        match self {
            // If we only have one entry there's only one possiblity
            CodeTree::Terminal(e) => {
                log::trace!("codetree parsed terminal, no input consumed");
                Ok((i, e.clone()))
            }
            // we have multiple entries - parse the number of bits needed and return the
            // matching one
            CodeTree::Node { left, right } => {
                let bits = self.bits()[0];
                let (rest, idx): (_, usize) = take(bits)(i)?;
                log::trace!("codetree parsed {}-bit prefix, value: {}", bits, idx);
                match idx.cmp(&left.len()) {
                    Ordering::Equal => right.parse(rest),
                    Ordering::Less => Ok((rest, left.get(idx).unwrap().clone())),
                    _ => panic!("Bad index {} for len {}", idx, self.len()[0]),
                }
            }
        }
    }

    ///! Iterates over the elements of the codetree, top to bottom, left to right.
    pub(super) fn iter(&'a self) -> Box<dyn Iterator<Item = &'a T> + 'a> {
        match self {
            Self::Terminal(v) => Box::new(once(v)),
            Self::Node { left, right } => Box::new(left.iter().chain(right.iter())),
        }
    }
}

impl<T> CodeTree<T>
where
    T: Clone + PartialEq,
{
    //! Finds the first instance of `needle` in the codetree, top to bottom, left to right
    pub(super) fn find(&self, needle: &T) -> Option<Vec<usize>> {
        match self {
            // in the simple case, this codetree is a single terminal
            Self::Terminal(v) => (*v == *needle).then_some(vec![0]),
            // otherwise, search through the node
            Self::Node { left, right } => left
                .iter()
                // first the left
                .position(|e| e == needle)
                .map(|i| vec![i + 1])
                // and failing that the right
                .or_else(|| match right.as_ref() {
                    // where we have to do a slightly ugly special case - we can't just recurse on
                    // `right` because we consider a terminal as part of "this" layer of the tree,
                    // not the next...
                    Self::Terminal(v) => (*v == *needle).then_some(vec![left.len()]),
                    Self::Node { .. } => right.find(needle).map(|mut next| {
                        next.insert(0, left.len());
                        next
                    }),
                }),
        }
    }
}

///! A macro to make it easier to construct a codetree
///! ```
///!   let c = codetree!(
///!       0 "foo";
///!       1 "bar";
///!       2,0 "baz";
///!   );
///!   assert_eq!(c.bits(), vec!(2, 0));
///! ````
macro_rules! codetree {
    ($($($loc:literal),+ => $val:expr$(, $cond:expr)?;)+) => {{
        let (mut one, mut two ,mut three) = (vec!(), vec!(), vec!());
        $(
            // gnarly - if $cond is present, use it as the condition to adding
            // this value, otherwise if (true).
            if ($($cond &&)? true) {
                // TODO: if metavariable expressions ever stabilise just use `count`
                let mut l = 0;
                $(
                    // TODO: Gross can we just ignore it somehow??
                    $loc;
                    l += 1;
                )+
                match l {
                    1 => {
                        if two.len() > 0 || three.len()> 0{
                            panic!("invalid codetree");
                        }
                        one.push($val);
                    },
                    2 => {
                        if three.len() > 0 {
                            panic!("stop");
                        }
                        two.push($val);
                    },
                    3 => {
                        three.push($val);
                    },
                    _ => {
                        panic!("Codetree must have 1-3 layers");
                    }
                }
            }
        )*
        CodeTree::from_vecs(vec!(one, two, three))

    }}
}
pub(crate) use codetree;

#[cfg(test)]
mod tests {
    use std::vec;

    use super::*;

    #[test]
    fn test_macro() {
        let ct = codetree!(
            0 => "hello";
            1 => "world";
        );
        assert_eq!(ct.bits(), vec!(1));
        let ct = codetree!(
            0     => "foo";
            1,0   => "bar";
            1,1   => "baz";
            1,2,0 => "boing";
        );
        assert_eq!(ct.bits(), vec!(1, 2, 0));
        let ct = codetree!(
            0 => "foo";
            1 => "bar", 1 == 1;
        );
        assert_eq!(ct.len(), vec!(2));
        let ct = codetree!(
            0 => "foo";
            1 => "bar", 3 == 4;
        );
        assert_eq!(ct.len(), vec!(1));
    }

    #[test]
    fn test_code_tree_len_bits() {
        let t = codetree!(
            0 => 1;
            1 => 2;
        );
        assert_eq!(t.len(), vec!(2));
        assert_eq!(t.bits(), vec!(1));

        let t = codetree!(
            0 => 1;
        );
        assert_eq!(t.len(), vec!(1));
        assert_eq!(t.bits(), vec!(0));

        let t = codetree!(
            0     => 1;
            1     => 2;
            2     => 3;
            3,0   => 4;
            3,1,0 => 5;
            3,1,1 => 6;
            3,1,2 => 7;
        );
        assert_eq!(t.len(), vec!(4, 2, 3));
        assert_eq!(t.bits(), vec! {2, 1, 2});
    }

    #[test]
    fn test_codetree_parse() {
        // Simple, 0-bit case.
        assert_eq!(
            codetree!(
                0 => 1;
            )
            .parse((&[0b1010_1010], 0)),
            Ok(((vec!(0b1010_1010u8).as_slice(), 0), 1))
        );
        // 1-bit cases
        let ct = codetree!(
            0 => 1;
            1 => 2;
        );
        assert_eq!(
            ct.parse((&[0b0000_0000], 0)),
            Ok(((vec!(0b0000_0000).as_slice(), 1), 1))
        );
        assert_eq!(
            ct.parse((&[0b1000_0000], 0)),
            Ok(((vec!(0b1000_0000).as_slice(), 1), 2))
        );
        // 2-bit case
        assert_eq!(
            codetree!(
                0 => 1;
                1 => 2;
                2 => 3;
            )
            .parse((&[0b0100_0000], 0)),
            Ok(((vec!(0b0100_0000).as_slice(), 2), 2))
        );

        // (0,1)-bit case
        assert_eq!(
            codetree!(
                0,0 => 1;
                0,1 => 2;
            )
            .parse((&[0b1000_0000], 0)),
            Ok(((vec!(0b1000_0000).as_slice(), 1), 2))
        );
        // (2,2)-bit case
        assert_eq!(
            codetree!(
                0 => 1;
                1 => 2;
                2 => 3;
                3,0 => 4;
                3,1 => 5;
                3,2 => 6;
                3,3 => 7;
            )
            .parse((&[0b1110_0000], 0)),
            Ok(((vec!(0b1110_0000).as_slice(), 4), 6))
        );
    }

    #[test]
    fn test_codetree_from_vecs() {
        assert_eq!(
            CodeTree::from_vecs(vec!(vec!(1, 2))),
            CodeTree::Node {
                left: vec!(1),
                right: CodeTree::Terminal(2).into(),
            },
        );
        assert_eq!(
            CodeTree::from_vecs(vec!(vec!(1, 2, 3))),
            CodeTree::Node {
                left: vec!(1, 2),
                right: CodeTree::Terminal(3).into(),
            },
        );

        assert_eq!(
            CodeTree::from_vecs(vec!(vec!(1, 2), vec!(3))),
            CodeTree::Node {
                left: vec!(1, 2),
                right: CodeTree::Node {
                    left: vec!(),
                    right: CodeTree::Terminal(3).into()
                }
                .into()
            }
        );

        assert_eq!(
            CodeTree::from_vecs(vec!(vec!(1), vec!(), vec!(3, 4))),
            CodeTree::Node {
                left: vec!(1),
                right: CodeTree::Node {
                    left: vec!(),
                    right: CodeTree::Node {
                        left: vec!(3),
                        right: CodeTree::Terminal(4).into(),
                    }
                    .into(),
                }
                .into()
            }
        );
    }

    #[test]
    fn test_codetree_insert() {
        assert_eq!(
            CodeTree::Terminal(1).insert(0, 2),
            CodeTree::Node {
                left: vec!(2),
                right: CodeTree::Terminal(1).into()
            }
        );
        assert_eq!(
            CodeTree::Terminal(1).insert(1, 2),
            CodeTree::Node {
                left: vec!(1),
                right: CodeTree::Terminal(2).into()
            }
        );
        assert_eq!(
            CodeTree::Node {
                left: vec!(1, 2),
                right: CodeTree::Terminal(3).into()
            }
            .insert(2, 4),
            CodeTree::Node {
                left: vec!(1, 2, 4),
                right: CodeTree::Terminal(3).into()
            }
        )
    }

    #[test]
    fn codetree_map() {
        assert_eq!(
            CodeTree::Terminal(1usize).map(|i| i.to_string()),
            CodeTree::Terminal("1".to_string()),
        )
    }

    #[test]
    fn codetree_iter() {
        for (ct, exp) in [
            (CodeTree::Terminal(1), vec![1]),
            (
                CodeTree::Node {
                    left: vec![0, 1],
                    right: CodeTree::Terminal(2).into(),
                },
                vec![0, 1, 2],
            ),
            (
                CodeTree::Node {
                    left: vec![0],
                    right: CodeTree::Node {
                        left: vec![1, 2, 3],
                        right: CodeTree::Terminal(4).into(),
                    }
                    .into(),
                },
                vec![0, 1, 2, 3, 4],
            ),
        ] {
            assert_eq!(ct.iter().cloned().collect::<Vec<_>>(), exp,);
        }
    }

    #[test]
    fn codetree_find() {
        assert!(CodeTree::Terminal(1).find(&2).is_none());
        assert_eq!(CodeTree::Terminal(1).find(&1), Some(vec![0]));
        assert_eq!(
            CodeTree::Node {
                left: vec![0, 1],
                right: CodeTree::Terminal(3).into()
            }
            .find(&3),
            Some(vec![2])
        );
        assert_eq!(
            CodeTree::Node {
                left: vec![],
                right: CodeTree::Node {
                    left: vec![],
                    right: CodeTree::Terminal(1).into()
                }
                .into()
            }
            .find(&1),
            Some(vec![0, 0])
        );
    }
}
