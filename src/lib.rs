//! An EXI implementation in Rust.
//!
//!     let mut buf = Vec::new();
//!     File::open(file)?.read_to_end(&mut buf)?;
//!     let decoded = exi::decoder::decode(&buf)?;
//!     fmt::println!("Decoded EXI version {} with {} events", decoded.header.version, decoded.body)
/// EXI Decoder
pub mod decoder;
mod util;
