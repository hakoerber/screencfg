use std::{path::PathBuf, process, time::Duration};

use i3::{Conn as _, EventType};

mod cli;
mod config;
mod error;
mod nonempty;
mod ordering;
mod output;
mod plan;
mod workspace;
mod workstation;

use self::{
    error::Error,
    ordering::ExternalOrdering,
    output::{
        ActiveState as OutputActiveState, Class as OutputClass,
        ConnectionState as OutputConnectionState, Name as OutputName, Output,
        Setting as OutputSetting, State as OutputState,
    },
    plan::Plan,
    workstation::Workstation,
};

#[cfg(test)]
mod tests;

#[derive(Debug, Clone, Copy)]
enum Setup {
    LaptopLeft,
    LaptopRight,
    LaptopOnly,
    ExternalOnly,
    Projector,
}

#[expect(clippy::print_stdout, reason = "high level function")]
fn manage_screens(
    config: Option<&config::Config>,
    debug: bool,
    dry_run: bool,
    diagram: bool,
    approach: cli::Approach,
    custom_external_ordering: Option<&str>,
) -> Result<(), Error> {
    let mut i3_connection = i3::connect()?;

    let outputs = Output::findall(&mut i3_connection)?;
    let workstation: Workstation<'_> = (&*outputs).try_into()?;

    let workspaces = i3_connection.workspaces()?;
    let workspaces =
        workspace::Workspaces::convert(workspaces, &outputs.iter().collect::<Vec<&Output>>())?;

    if debug {
        println!("=== workstation setup:");

        println!("=== laptop output:");
        match workstation.laptop {
            Some(output) => {
                println!(
                    "{name} ({state})",
                    name = output.name,
                    state = output.connection_state
                );
            }
            None => println!("none"),
        }

        println!("=== external outputs:");
        match workstation.externals {
            Some(ref externals) => {
                for output in externals {
                    println!(
                        "{name} ({state})",
                        name = output.name,
                        state = output.connection_state
                    );
                }
            }
            None => println!("none"),
        }
        println!("=== i3 workspaces:");
        for workspace in &workspaces.0 {
            println!("  - {workspace}");
        }
        println!();
    }

    i3_connection.command(i3::Command::Nop)?;

    let external_ordering = match custom_external_ordering {
        Some(order) => ExternalOrdering::parse_from_str(
            order,
            match workstation.externals {
                Some(ref externals) => externals.len(),
                None => 0,
            },
        )?,
        None => ExternalOrdering::Default,
    };

    let plan = match approach {
        cli::Approach::LaptopLeft => {
            workstation.plan(Setup::LaptopLeft, &workspaces, &external_ordering)?
        }
        cli::Approach::LaptopRight => {
            workstation.plan(Setup::LaptopRight, &workspaces, &external_ordering)?
        }
        cli::Approach::LaptopOnly => {
            workstation.plan(Setup::LaptopOnly, &workspaces, &external_ordering)?
        }
        cli::Approach::ExternalOnly => {
            workstation.plan(Setup::ExternalOnly, &workspaces, &external_ordering)?
        }
        cli::Approach::Projector => {
            workstation.plan(Setup::Projector, &workspaces, &external_ordering)?
        }
        cli::Approach::Best => workstation
            .plan(Setup::LaptopLeft, &workspaces, &external_ordering)
            .or_else(|_| workstation.plan(Setup::LaptopOnly, &workspaces, &external_ordering))
            .or_else(|_| workstation.plan(Setup::ExternalOnly, &workspaces, &external_ordering))
            .map_err(|_err| Error::Plan("no plan fit with \"best\" strategy".into()))?,
    };

    if debug {
        println!("{plan}");
    }

    if diagram {
        let mut buf = String::new();
        plan.diagram(&mut buf)
            .map_err(|err| Error::Generic(err.to_string().into()))?;

        println!("{buf}\n");
    }

    let commands = plan.commands();

    if dry_run {
        println!("dry run mode:");
    } else {
        println!("applying changes:");
    }

    for command in &commands {
        println!("- {command}");
    }

    if !dry_run {
        Plan::apply(&mut i3_connection, commands)?;
    }

    if let Some(post_commands) = config.and_then(|c| c.post_commands.as_ref()) {
        for command in post_commands {
            println!("executing post command \"{command}\"");
            let output = process::Command::new("bash")
                .arg("-c")
                .arg(command)
                .output()
                .map_err(|e| {
                    Error::Generic(
                        format!("post command \"{command}\" invocation failed: {e}").into(),
                    )
                })?;

            if !output.status.success() {
                return Err(Error::Generic(
                    format!(
                        "post command \"{command}\" failed: {stderr}",
                        stderr = String::from_utf8(output.stderr)
                            .unwrap_or_else(|_| "stderr invalid utf8".to_owned())
                    )
                    .into(),
                ));
            }
        }
    }

    Ok(())
}

#[expect(clippy::print_stderr, reason = "main")]
fn run() -> Result<(), Error> {
    let args = cli::Cli::parse();

    match args.subcommand {
        cli::Cmd::Set(set_options) => {
            let config = config::find(args.config.map(|path| PathBuf::from(path)).as_deref())?;

            manage_screens(
                config.as_ref(),
                args.debug,
                set_options.dry_run,
                set_options.diagram,
                set_options.setup,
                set_options.custom_external_ordering.as_deref(),
            )?;
        }
        cli::Cmd::Watch(watch_options) => {
            let config = config::find(args.config.map(|path| PathBuf::from(path)).as_deref())?;

            let i3_connection = i3::connect()?;

            let Err(err) = i3::start_event_listener::<_, Error>(
                i3_connection,
                Duration::from_millis(1000),
                &[EventType::Output],
                |event| {
                    eprintln!("received event from i3: {event}");
                    manage_screens(
                        config.as_ref(),
                        args.debug,
                        watch_options.dry_run,
                        watch_options.diagram,
                        watch_options.setup,
                        watch_options.custom_external_ordering.as_deref(),
                    )?;

                    Ok::<(), Error>(())
                },
            );

            eprintln!("{err}");
        }
    }

    Ok(())
}

#[expect(clippy::print_stderr, reason = "main")]
fn main() -> process::ExitCode {
    process::ExitCode::from(match run() {
        Ok(()) => 0,
        Err(e) => {
            eprintln!("{e}");
            1
        }
    })
}
