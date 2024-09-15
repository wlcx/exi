//! An EXI implementation in Rust.
//!
//!     use std::io::Read;
//!     use std::fs::File;
//!
//!     let mut buf = Vec::new();
//!     File::open("test/notebook.xml.exi".to_string()).unwrap().read_to_end(&mut buf).unwrap();
//!     let decoded = exi::decoder::decode(&buf).unwrap();
//!     println!("Decoded EXI version {:?} with {} events", decoded.header.version, decoded.body.len())
/// EXI Decoder
pub mod decoder;
mod util;
