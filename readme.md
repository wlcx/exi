# EXI

A Rust [EXI] (Efficient eXtensible Interchange) implementation. EXI is a very compact
binary XML representation.

This implementation does not need many dependencies and doesn't include any `unsafe` Rust.

## Status
> ⚠️ Only very basic decoding functionality currently implemented. Will crash, eat your laundry, etc when given any real-world EXI input.

### Decoder
| Item | Spec Link | State/Comments |
|------|-----------|----------------|
| Schema-less decoding | https://www.w3.org/TR/2014/REC-exi-20140211/#builtinGrammars | Decoding partially implemented |
| Options Decoding | https://www.w3.org/TR/2014/REC-exi-20140211/#options | ❌ Unimplemented, decoder will error if options are present in header |
| Schema-informed decoding | https://www.w3.org/TR/2014/REC-exi-20140211/#informedGrammars | ❌ Unimplemented |
| EXI Compression | https://www.w3.org/TR/2014/REC-exi-20140211/#compression | ❌ Unimplemented |

### Encoder
Currently unimplemented

## Helpful Resources
- The [EXI Spec] is, for the most part, readable and complete - this implementation was
written solely with reference to it and the primer (see below), without having to inspect
already-existing implementations.
- The [EXI Primer](https://www.w3.org/TR/2014/WD-exi-primer-20140424/) is a reasonable
high level introduction to the concepts, although it perhaps under-explains some important
aspects. In particular, the example step-by-step encoding/decoding are great, and I wish
more specs included them!

[EXI]: https://www.w3.org/XML/EXI/
[EXI Spec]: https://www.w3.org/TR/2014/REC-exi-20140211/