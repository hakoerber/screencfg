use std::time::Duration;

use tokio::{sync::mpsc, time};
use tokio_util::sync::CancellationToken;

pub enum ReadResult<T> {
    Ok(T),
    Closed,
    Cancelled,
}

pub struct Debouncer {
    duration: Duration,
}

impl Debouncer {
    pub fn new(duration: Duration) -> Self {
        Self { duration }
    }

    /// * If the channel gets closed after we received an event, but before the event is handled,
    ///   we will **not** run `f()`.
    ///
    /// * `f()` will be called with the **first** received value in the debounce duration. All other
    ///   values will be discarded
    pub async fn read_from_channel<T>(
        &self,
        channel: &mut mpsc::Receiver<T>,
        cancellation_token: &CancellationToken,
    ) -> ReadResult<T>
    where
        T: std::fmt::Display,
    {
        tracing::debug!(
            "reading from channel with debounce duration {:?}",
            self.duration
        );
        let sleep = time::sleep(self.duration);
        tokio::pin!(sleep);

        match channel.recv().await {
            Some(v) => loop {
                return tokio::select! {
                    biased;
                    () = cancellation_token.cancelled() => {
                        tracing::debug!("debouncer received cancellation signal");
                        ReadResult::Cancelled
                    },
                    v = channel.recv() => {
                        match v {
                            Some(v) => {
                                tracing::debug!("debouncer received \"{v}\" during debounce duration, dropping");
                                drop(v);
                                continue;
                            }
                            None => {
                                tracing::debug!("debouncer notices closed channel, aborting");
                                ReadResult::Closed
                            }
                        }
                    },
                    () = &mut sleep => {
                        tracing::debug!("debounce duration elapsed, emitting \"{v}\"");
                        ReadResult::Ok(v)
                    },
                };
            },
            None => {
                tracing::debug!("debouncer called with closed channel, aborting");
                ReadResult::Closed
            }
        }
    }
}
