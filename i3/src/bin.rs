#![allow(unused_crate_dependencies)]

use std::{error::Error, time::Duration};

use i3::{self, EventType, start_event_listener};

fn main() -> Result<(), Box<dyn Error>> {
    let i3_connection = i3::connect()?;

    let Err(err) = start_event_listener(
        i3_connection,
        Duration::from_millis(1000),
        &[EventType::Output, EventType::Workspace],
        |event| {
            println!("{event:?}");
            Err(i3::Error::Protocol("too".into()))
        },
    );

    println!("{err:?}");

    Ok(())
}
