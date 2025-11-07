use clap::{Args, Parser, Subcommand, ValueEnum};

#[derive(Clone, Copy, ValueEnum)]
pub(crate) enum Setup {
    LaptopLeft,
    LaptopRight,
    LaptopOnly,
    ExternalOnly,
    Projector,
}

#[derive(Clone, Copy, Args)]
#[group(multiple = false, required = true)]
pub(crate) struct Approach {
    #[arg(long)]
    pub(crate) setup: Option<Setup>,

    #[arg(long)]
    pub(crate) best: bool,
}

#[derive(Parser)]
#[command(version, about)]
pub(crate) struct Cli {
    #[command(subcommand)]
    pub(crate) subcommand: Cmd,

    #[arg(long, global = true)]
    pub(crate) config: Option<String>,

    #[arg(long, global = true)]
    pub(crate) debug: bool,
}

impl Cli {
    pub(crate) fn parse() -> Self {
        <Self as clap::Parser>::parse()
    }
}

#[derive(Subcommand, Clone)]
pub(crate) enum Cmd {
    Set(SetOptions),
    Watch(WatchOptions),
}

#[derive(Args, Clone)]
pub(crate) struct SetOptions {
    #[command(flatten)]
    pub(crate) approach: Approach,

    #[arg(long)]
    pub(crate) dry_run: bool,

    #[arg(long)]
    pub(crate) diagram: bool,
}

#[derive(Args, Clone)]
pub(crate) struct WatchOptions {
    #[command(flatten)]
    pub(crate) approach: Approach,

    #[arg(long)]
    pub(crate) dry_run: bool,

    #[arg(long)]
    pub(crate) diagram: bool,
}
