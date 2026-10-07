#![allow(unused_crate_dependencies)]

use std::{error::Error, time::Duration};

use i3::{self, EventType, start_event_listener};

#[tokio::main(flavor = "current_thread")]
async fn main() -> Result<(), Box<dyn Error>> {
    let i3_connection = i3::connect().await?;

    let Err(err) = start_event_listener(
        i3_connection,
        Duration::from_millis(1000),
        &[EventType::Output, EventType::Workspace],
        async |event| -> Result<(), i3::Error> {
            println!("{event:?}");
            Ok(())
        },
    )
    .await;

    println!("{err:?}");

    Ok(())
}
