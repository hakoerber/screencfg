use clap::{Args, Parser, Subcommand, ValueEnum};

#[derive(Clone, Copy, ValueEnum)]
pub(crate) enum Approach {
    LaptopLeft,
    LaptopRight,
    LaptopOnly,
    ExternalOnly,
    Projector,
    Best,
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
    #[expect(
        clippy::same_name_method,
        reason = "expose parsing without needing to import trait"
    )]
    pub(crate) fn parse() -> Self {
        <Self as Parser>::parse()
    }
}

#[derive(Subcommand, Clone)]
pub(crate) enum Cmd {
    Set(SetOptions),
    Watch(WatchOptions),
}

#[derive(Args, Clone)]
pub(crate) struct SetOptions {
    #[arg(long)]
    pub(crate) setup: Approach,

    #[arg(long)]
    pub(crate) custom_external_ordering: Option<String>,

    #[arg(long)]
    pub(crate) dry_run: bool,

    #[arg(long)]
    pub(crate) diagram: bool,
}

#[derive(Args, Clone)]
pub(crate) struct WatchOptions {
    #[arg(long)]
    pub(crate) setup: Approach,

    #[arg(long)]
    pub(crate) custom_external_ordering: Option<String>,

    #[arg(long)]
    pub(crate) dry_run: bool,

    #[arg(long)]
    pub(crate) diagram: bool,
}
