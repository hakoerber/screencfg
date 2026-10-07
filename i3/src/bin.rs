#![expect(unused_crate_dependencies, reason = "this detection sucks for bins")]
#![expect(clippy::print_stdout, reason = "its a binary")]

use std::{error::Error, time::Duration};

use i3::{self, Conn as _, EventType};

#[tokio::main(flavor = "current_thread")]
async fn main() -> Result<(), Box<dyn Error>> {
    let i3_connection = i3::Connection::connect().await?;

    let subscription = i3_connection
        .subscribe(&[EventType::Output, EventType::Workspace])
        .await?;

    let Err(err) = subscription
        .start_event_listener(
            Duration::from_millis(1000),
            async |event| -> Result<(), i3::Error> {
                println!("{event}");
                Ok(())
            },
        )
        .await;

    println!("{err}");

    Ok(())
}
