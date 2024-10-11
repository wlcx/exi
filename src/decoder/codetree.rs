use std::cmp::Ordering;

use nom::bits::complete::take;
use nom::IResult;

use crate::util::{ilog2_ceil, BitInput};

// Describes the tree of event codes in an EXI grammar.
// `CodeTree<T>`s always:
// - Consist of 1+ levels
// - Each level having 0+ left `T`s
// - And have exactly one right entry, which is either another CodeTree, or a terminal `T``
#[derive(Clone, PartialEq, Debug)]
pub(super) enum CodeTree<T: Clone> {
    Terminal(T),
    Node {
        left: Vec<T>,
        right: Box<CodeTree<T>>,
    },
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
                        .map(|(i, t)| (format!("{}", t), format!("{}{}", prefix, i))),
                );
                match right.as_ref() {
                    Self::Node { left: _, right: _ } => {
                        out.append(&mut right._pprint(&format!("{}{}.", prefix, left.len())));
                    }
                    Self::Terminal(t) => {
                        out.push((format!("{}", t), format!("{}{}", prefix, left.len())));
                    }
                }
            }
            Self::Terminal(t) => out.push((format!("{}", t), format!("{}0", prefix))),
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

impl<T: Clone> CodeTree<T> {
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

    // Insert an element at the index described by `index`. Returns a copy of codetree
    // with the element inserted
    pub(super) fn insert(&mut self, index: usize, v: T) -> Self {
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
                left.insert(idx, v);
                CodeTree::Node {
                    left: left.to_vec(),
                    right: right.clone(),
                }
            }
        }
    }

    // Given this codetree, produce the next event from bitstream i
    pub(super) fn parse<'a>(&self, i: BitInput<'a>) -> IResult<BitInput<'a>, T> {
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
}

#[cfg(test)]
mod tests {
    use std::vec;

    use super::*;

    #[test]
    fn test_code_tree_len_bits() {
        let t = CodeTree::Node {
            left: vec![1],
            right: CodeTree::Terminal(2).into(),
        };
        assert_eq!(t.len(), vec!(2));
        assert_eq!(t.bits(), vec!(1));

        let t = CodeTree::Terminal(1);
        assert_eq!(t.len(), vec!(1));
        assert_eq!(t.bits(), vec!(0));

        let t = CodeTree::Node {
            left: vec![1, 2, 3],
            right: CodeTree::Node {
                left: vec![4],
                right: CodeTree::Node {
                    left: vec![5, 6],
                    right: CodeTree::Terminal(7).into(),
                }
                .into(),
            }
            .into(),
        };
        assert_eq!(t.len(), vec!(4, 2, 3));
        assert_eq!(t.bits(), vec! {2, 1, 2});
    }

    #[test]
    fn test_codetree_parse() {
        // Simple, 0-bit case.
        assert_eq!(
            CodeTree::Terminal(1).parse((&[0b1010_1010], 0)),
            Ok(((vec!(0b1010_1010u8).as_slice(), 0), 1))
        );
        // 1-bit cases
        assert_eq!(
            CodeTree::Node {
                left: vec!(1),
                right: CodeTree::Terminal(2).into()
            }
            .parse((&[0b0000_0000], 0)),
            Ok(((vec!(0b0000_0000).as_slice(), 1), 1))
        );
        assert_eq!(
            CodeTree::Node {
                left: vec!(1),
                right: CodeTree::Terminal(2).into()
            }
            .parse((&[0b1000_0000], 0)),
            Ok(((vec!(0b1000_0000).as_slice(), 1), 2))
        );
        // 2-bit case
        assert_eq!(
            CodeTree::Node {
                left: vec!(1, 2),
                right: CodeTree::Terminal(3).into()
            }
            .parse((&[0b0100_0000], 0)),
            Ok(((vec!(0b0100_0000).as_slice(), 2), 2))
        );

        // (0,1)-bit case
        assert_eq!(
            CodeTree::Node {
                left: vec!(),
                right: CodeTree::Node {
                    left: vec!(1),
                    right: CodeTree::Terminal(2).into(),
                }
                .into()
            }
            .parse((&[0b1000_0000], 0)),
            Ok(((vec!(0b1000_0000).as_slice(), 1), 2))
        );
        // (2,2)-bit case
        assert_eq!(
            CodeTree::Node {
                left: vec!(1, 2, 3),
                right: CodeTree::Node {
                    left: vec!(4, 5, 6),
                    right: CodeTree::Terminal(7).into(),
                }
                .into()
            }
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
}
