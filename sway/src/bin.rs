#![expect(unused_crate_dependencies, reason = "this detection sucks for bins")]
#![expect(clippy::print_stdout, reason = "its a binary")]

use std::{error::Error, time::Duration};

use sway::{self, Conn as _, EventType};

#[tokio::main(flavor = "current_thread")]
async fn main() -> Result<(), Box<dyn Error>> {
    let socket_path = sway::get_socket_path()?;

    let sway_connection = sway::connect(&socket_path).await?;

    let subscription = sway_connection
        .subscribe(&[EventType::Output, EventType::Workspace])
        .await?;

    let Err(err) = subscription
        .start_event_listener(
            Duration::from_millis(1000),
            async |event| -> Result<(), sway::Error> {
                println!("{event}");
                Ok(())
            },
        )
        .await;

    println!("{err}");

    Ok(())
}
