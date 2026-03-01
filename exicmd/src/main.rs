use exi::{
    self,
    decoder::{options::Options, quickxml::QuickXMLIterator},
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
        /// Number of spaces to indent pretty output)
        #[arg(short, long)]
        indent: Option<u8>,
        /// Decode with the "preserve prefixes" EXI option enabled. Only valid when the EXI document doesn't have embedded options.
        #[arg(short = 'p', long)]
        preserve_prefixes: bool,
        /// Decode with the "preserve comments" EXI option enabled. Only valid when the EXI document doesn't have embedded options.
        #[arg(short = 'c', long)]
        preserve_comments: bool,
        /// Decode with the "preserve processing instructions" EXI option enabled. Only valid when the EXI document doesn't have embedded options.
        #[arg(short = 'i', long)]
        preserve_pis: bool,
        /// Decode with the "preserve DTD" EXI option enabled. Only valid when the EXI document doesn't have embedded options.
        #[arg(short = 'd', long)]
        preserve_dtd: bool,
        /// Decode with the "fragment" EXI option enabled. Only valid when the EXI document doesn't have embedded options.
        #[arg(short, long)]
        fragment: bool,
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
            indent,
            preserve_prefixes,
            preserve_comments,
            preserve_pis,
            preserve_dtd,
            fragment,
        } => {
            let mut buf = Vec::new();
            let mut input: Box<dyn BufRead> = match in_file {
                None => Box::new(BufReader::new(io::stdin())),
                Some(f) => Box::new(BufReader::new(File::open(f)?)),
            };
            input.read_to_end(&mut buf)?;
            let output: Box<dyn Write> = match out_file {
                None => Box::new(BufWriter::new(io::stdout())),
                Some(f) => Box::new(BufWriter::new(File::create_new(f)?)),
            };
            let mut w = match indent {
                Some(n) => quick_xml::Writer::new_with_indent(output, b' ', n.into()),
                None => quick_xml::Writer::new(output),
            };
            let opts = if preserve_prefixes
                || preserve_comments
                || preserve_pis
                || preserve_dtd
                || fragment
            {
                Some(
                    Options::default()
                        .with_preserve_prefixes(preserve_prefixes)
                        .with_preserve_comments(preserve_comments)
                        .with_preserve_pis(preserve_pis)
                        .with_preserve_dtd(preserve_dtd)
                        .with_fragment(fragment),
                )
            } else {
                None
            };
            for ev in exi::decoder::decode(&buf, opts)?
                .body
                .into_iter()
                .to_quick_xml()
            {
                w.write_event(ev)?;
            }
        }
    }
    Ok(())
}
