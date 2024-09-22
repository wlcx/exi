use exi::{
    self,
    decoder::quickxml::{self, QuickXMLIterator},
};
use log::{self, LevelFilter};
use std::{
    fs::File,
    io::{self, BufRead, BufReader, BufWriter, Read, Write},
    path::PathBuf,
};
use thiserror::Error;

use clap::{Parser, Subcommand};

#[derive(Parser)]
#[command(version)]
/// An EXI (Efficient eXtensible Interchange) utility.
///
/// A command line tool for decoding and (eventually) encoding EXI-encoded XML documents.
struct Cli {
    #[command(subcommand)]
    command: Commands,

    /// Logging verbosity. Chain for more detail.
    #[arg(short, long, action = clap::ArgAction::Count)]
    verbose: u8,
}

#[derive(Subcommand)]
enum Commands {
    /// Decode an EXI-encoded document
    Decode {
        /// File to read from. Defaults to stdin.
        in_file: Option<PathBuf>,
        /// File to output to. Defaults to stdout.
        out_file: Option<PathBuf>,
        /// Pretty print output (indented)
        #[arg(short, long)]
        pretty: bool,
    },
}

#[derive(Error, Debug)]
enum Error {
    #[error("bad usage: {0}")]
    BadUsage(String),
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let cli = Cli::parse();
    let level = match cli.verbose {
        0 => LevelFilter::Warn,
        1 => LevelFilter::Info,
        2 => LevelFilter::Debug,
        3 => LevelFilter::Trace,
        _ => {
            return Err(Error::BadUsage("verbose can only be specified 0 to 3 times".into()).into())
        }
    };
    env_logger::builder().filter_level(level).init();
    match cli.command {
        Commands::Decode {
            in_file,
            out_file,
            pretty,
        } => {
            let mut buf = Vec::new();
            let mut input: Box<dyn BufRead> = match in_file {
                None => Box::new(BufReader::new(io::stdin())),
                Some(f) => Box::new(BufReader::new(File::open(f)?)),
            };
            input.read_to_end(&mut buf)?;
            let output: Box<dyn Write> = match out_file {
                None => Box::new(BufWriter::new(io::stdout())),
                Some(f) => Box::new(BufWriter::new(File::open(f)?)),
            };
            let mut w = if pretty {
                quick_xml::Writer::new_with_indent(output, b' ', 4)
            } else {
                quick_xml::Writer::new(output)
            };
            for ev in exi::decoder::decode(&buf)?.body.into_iter().to_quick_xml() {
                w.write_event(ev)?;
            }
        }
    }
    Ok(())
}
