use std::{process, time::Duration};

use tracing::Level;

use sway::{Conn as _, EventType};

mod cli;
mod error;
mod ordering;
mod output;
mod plan;
mod workspace;
mod workstation;

use crate::workstation::UnorderedWorkstation;

use self::{
    error::Error,
    ordering::MonitorOrdering,
    output::{
        ActiveState as OutputActiveState, Class as OutputClass, Name as OutputName, Output,
        Setting as OutputSetting,
    },
    plan::Plan,
    workspace::Setting as WorkspaceSetting,
    workstation::Workstation,
};

#[cfg(test)]
mod tests;

#[derive(Debug, Clone, Copy, derive_more::Display)]
#[cfg_attr(any(test, feature = "testing"), derive(Eq, PartialEq))]
enum Setup {
    LaptopLeft,
    LaptopRight,
    LaptopOnly,
    MonitorOnly,
    Projector,
}

#[expect(clippy::print_stdout, reason = "high level function")]
async fn manage_screens(
    connection: &mut sway::Connection,
    debug: bool,
    dry_run: bool,
    diagram: bool,
    approach: cli::Approach,
    custom_monitor_ordering: Option<&str>,
) -> Result<(), Error> {
    connection.command(sway::Command::Nop).await?;

    let outputs = Output::findall(connection).await?;

    let monitor_count = outputs
        .iter()
        .filter(|output| output.class == output::Class::Monitor)
        .count();

    let monitor_ordering = match custom_monitor_ordering {
        Some(order) => MonitorOrdering::parse_from_str(order, monitor_count)?,
        None => MonitorOrdering::Default,
    };

    let workstation = UnorderedWorkstation::from_outputs(&outputs)?.order(&monitor_ordering);

    let workspaces = connection.workspaces().await?;
    let workspaces =
        workspace::Workspaces::convert(workspaces, &outputs.iter().collect::<Vec<&Output>>())?;

    if debug {
        println!("=== workstation setup:");

        match workstation {
            Workstation::LaptopOnly { laptop } => {
                println!("  found only a laptop {name}", name = laptop.name);
            }
            Workstation::MonitorOnly { ref monitors } => {
                println!("  found only monitor outputs:");
                for monitor in monitors {
                    println!("  - {monitor}");
                }
            }
            Workstation::Mixed {
                laptop,
                ref monitors,
            } => {
                println!(
                    "  found laptop {name} and monitor outputs {monitors}",
                    name = laptop.name,
                    monitors = monitors
                        .iter()
                        .map(|output| output.name.as_str())
                        .collect::<Vec<&str>>()
                        .join(", ")
                );
            }
        }

        println!("=== sway workspaces:");
        for workspace in &workspaces.0 {
            println!("  - {workspace}");
        }
    }

    let plan = match approach {
        cli::Approach::LaptopLeft => workstation.plan(Setup::LaptopLeft, &workspaces)?,
        cli::Approach::LaptopRight => workstation.plan(Setup::LaptopRight, &workspaces)?,
        cli::Approach::LaptopOnly => workstation.plan(Setup::LaptopOnly, &workspaces)?,
        cli::Approach::MonitorOnly => workstation.plan(Setup::MonitorOnly, &workspaces)?,
        cli::Approach::Projector => workstation.plan(Setup::Projector, &workspaces)?,
        cli::Approach::Best => {
            let plan = workstation
                .plan(Setup::LaptopLeft, &workspaces)
                .or_else(|_| workstation.plan(Setup::LaptopOnly, &workspaces))
                .or_else(|_| workstation.plan(Setup::MonitorOnly, &workspaces))
                .map_err(|_err| Error::NoBestStrategyFit)?;

            if debug {
                println!("best heuristic: selected the {} setup", plan.setup);
            }
            plan
        }
    };

    if debug {
        println!("{plan}");
    }

    if diagram {
        let mut buf = String::new();
        plan.diagram(&mut buf).map_err(|err| Error::Diagram(err))?;

        println!("{buf}\n");
    }

    let commands = plan.commands();

    if commands.is_empty() {
        println!("no changes");
    } else {
        if dry_run {
            println!("dry run, would be applying changes:");
        } else {
            println!("applying changes:");
        }
        for command in &commands {
            println!("- {command}");
        }
    }

    if !dry_run {
        Plan::apply(connection, commands).await?;
    }

    Ok(())
}

fn configure_tracing(level: Level) -> impl tracing::Subscriber {
    tracing_subscriber::fmt()
        .event_format(tracing_subscriber::fmt::format().compact())
        .with_timer(tracing_subscriber::fmt::time::SystemTime)
        .with_span_events(tracing_subscriber::fmt::format::FmtSpan::NONE)
        .with_ansi(true)
        .with_max_level(level)
        .with_target(true)
        .finish()
}

#[expect(clippy::print_stderr, reason = "main")]
async fn run() -> Result<(), Error> {
    let args = cli::Cli::parse();

    let subscriber = configure_tracing(if args.debug {
        Level::DEBUG
    } else {
        Level::INFO
    });

    tracing::subscriber::set_global_default(subscriber)?;

    let socket_path = sway::get_socket_path()?;
    tracing::debug!(%socket_path, "socket path found");

    match args.subcommand {
        cli::Cmd::Set(set_options) => {
            let mut connection = sway::connect(&socket_path).await?;

            manage_screens(
                &mut connection,
                args.debug,
                set_options.dry_run,
                set_options.diagram,
                set_options.setup,
                set_options.custom_monitor_ordering.as_deref(),
            )
            .await?;
        }
        cli::Cmd::Watch(watch_options) => {
            // two connections, as we do not want to mangle subscriptions with commands
            let subscribe_connection = sway::connect(&socket_path).await?;
            let mut command_connection = sway::connect(&socket_path).await?;

            let mut setter = async || {
                manage_screens(
                    &mut command_connection,
                    args.debug,
                    watch_options.dry_run,
                    watch_options.diagram,
                    watch_options.setup,
                    watch_options.custom_monitor_ordering.as_deref(),
                )
                .await
            };

            setter().await?;

            let subscription = subscribe_connection.subscribe(&[EventType::Output]).await?;

            let Err(err) = subscription
                .start_event_listener::<_, Error>(Duration::from_millis(2000), async move |event| {
                    eprintln!("received event from sway: {event}");
                    match setter().await {
                        Ok(()) => (),
                        Err(err) => match err {
                            Error::WorkstationWithoutScreens => {
                                eprintln!("no screens found, skipping setup")
                                // It can happen that all screens are disconnected. in that case, we do *not* fail hard,
                                // but just do nothing. If a screen is connected again, we can handle it.
                            }
                            _ => return Err(err),
                        },
                    }

                    Ok::<(), Error>(())
                })
                .await;

            eprintln!("{err}");
        }
    }

    Ok(())
}

enum MainResult {
    Success,
    Failure(Error),
}

#[expect(clippy::print_stderr, reason = "needed to print failures")]
impl process::Termination for MainResult {
    fn report(self) -> process::ExitCode {
        match self {
            Self::Success => process::ExitCode::SUCCESS,
            Self::Failure(e) => {
                eprintln!("Error: {e}");
                e.into()
            }
        }
    }
}

#[tokio::main(flavor = "current_thread")]
async fn main() -> MainResult {
    match run().await {
        Ok(()) => MainResult::Success,
        Err(err) => MainResult::Failure(err),
    }
}
