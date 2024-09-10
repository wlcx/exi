use exi;
use log::{self, LevelFilter};
use std::{fs::File, io::Read, path::PathBuf};
use thiserror::Error;

use clap::{Parser, Subcommand};

#[derive(Parser)]
#[command(version, about)]
struct Cli {
    #[command(subcommand)]
    command: Commands,

    /// Logging verbosity. Chain for more detail.
    #[arg(short, long, action = clap::ArgAction::Count)]
    verbose: u8,
}

#[derive(Subcommand)]
enum Commands {
    Decode { file: PathBuf },
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
        Commands::Decode { file } => {
            let mut buf = Vec::new();
            File::open(file)?.read_to_end(&mut buf)?;
            for e in exi::decoder::decode(&buf)?.body {
                log::info!("{:?}", e);
            }
        }
    }
    Ok(())
}
