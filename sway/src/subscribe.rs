use std::{convert::Infallible, time::Duration};

use tokio::{sync::mpsc, task::JoinHandle};
use tokio_util::sync::CancellationToken;

use super::{Error, EventPayload, Response, ResponseType, connection::Connection};

pub enum TaskResult<T, E> {
    Ok(T),
    Err(E),
    Cancelled,
}

impl<T, Es, Et> From<Result<T, Es>> for TaskResult<T, Et>
where
    Es: Into<Et>,
{
    fn from(value: Result<T, Es>) -> Self {
        match value {
            Ok(v) => Self::Ok(v),
            Err(err) => Self::Err(err.into()),
        }
    }
}

enum ReadResult<T, E> {
    Ok(T),
    Err(E),
    Closed,
}

impl<T, Es, Et> From<Result<T, Es>> for ReadResult<T, Et>
where
    Es: Into<Et>,
{
    fn from(value: Result<T, Es>) -> Self {
        match value {
            Ok(v) => Self::Ok(v),
            Err(err) => Self::Err(err.into()),
        }
    }
}

pub trait Subscribe
where
    Self: Sized,
{
    fn listen(
        &mut self,
        sender: mpsc::Sender<Result<EventPayload, Error>>,
        debounce_time: Duration,
        cancellation_token: CancellationToken,
    ) -> impl Future<Output = TaskResult<Infallible, Error>>;
}

pub struct Subscription {
    connection: Connection,
}

impl Subscription {
    pub(crate) fn new(connection: Connection) -> Self {
        Self { connection }
    }

    /// This takes ownership of self because event handling
    /// should have a separate connection, as it fucks with request ordering
    ///
    /// <https://swaywm.org/docs/ipc.html#_events>
    ///
    /// > As soon as you subscribe to an event, it is not guaranteed any longer
    /// > that the requests to sway are processed in order. This means, the
    /// > following situation can happen: You send a GET_WORKSPACES request
    /// > but you receive a "workspace" event before receiving the reply to
    /// > GET_WORKSPACES. If your program does not want to cope which such kinds
    /// > of race conditions (an event based library may not have a problem here),
    /// > I suggest you create a separate connection to receive events.
    pub async fn start_event_listener<F, E>(
        mut self,
        debounce_time: Duration,
        mut handler: F,
    ) -> Result<Infallible, E>
    where
        F: AsyncFnMut(EventPayload) -> Result<(), E>,
        E: From<Error>,
        // this can be enabled when async_fn_traits is stabilized:
        //
        // for<'a> F::CallRefFuture<'a>: Send,
    {
        let (tx, mut rx) = mpsc::channel(100);
        let cancellation_token = CancellationToken::new();

        let event_subscriber = {
            let cancellation_token = cancellation_token.clone();
            tokio::spawn(async move { self.listen(tx, debounce_time, cancellation_token).await })
        };

        // this can be enabled when async_fn_traits is stabilized, to run the handler
        // in a separate tokio task:
        //
        // let event_handler_result = tokio::spawn(async move {
        //     loop {
        //         match rx.recv().await {
        //             Some(event) => {
        //                 let event = event?;
        //                 let handler_result = handler(event).await;
        //                 if let Err(err) = handler_result {
        //                     break Err(err);
        //                 }
        //             }
        //             None => break Ok(()),
        //         }
        //     }
        // })
        // .await
        // .expect("event handler thread to not panic");

        let event_handler_result: ReadResult<Infallible, E> = loop {
            match rx.recv().await {
                Some(event) => {
                    let event = event?;
                    let handler_result = handler(event).await;
                    if let Err(err) = handler_result {
                        break Err(err).into();
                    }
                }
                None => break ReadResult::Closed,
            }
        };

        // Event handler finished, we can cancel the subscriber
        cancellation_token.cancel();

        // Wait for subscriber to finish
        let event_subscriber_result = event_subscriber
            .await
            .expect("event subscriber thread to not panic");

        // Now we have a few scenarios, each being some error case:
        //
        // 1. The handler encountered an error. clean up and return it
        // 2. The subscriber encountered an error. clean up and return it
        // 3. Both the handler *and* the subscriber encountered an error. This could happen if the
        //    subscriber encountered an error during cancellation. This points to some implementation
        //    error and should never happen, so we panic
        // 4. None encountered an error. Why are they finished, then? Implementation error, panic.

        #[expect(clippy::panic, reason = "we panic for programming errors")]
        Err(match (event_handler_result, event_subscriber_result) {
            (ReadResult::Closed, TaskResult::Cancelled) => {
                panic!("threads finished unexpectedly without an error")
            }
            (ReadResult::Closed, TaskResult::Err(err)) => err.into(),
            (ReadResult::Err(err), TaskResult::Cancelled) => err,
            (ReadResult::Err(_err1), TaskResult::Err(_err2)) => {
                panic!("teardown of subscriber seems to have produced an error")
            }
        })
    }
}

impl Subscribe for Subscription {
    async fn listen(
        &mut self,
        sender: mpsc::Sender<Result<EventPayload, Error>>,
        debounce_time: Duration,
        cancellation_token: CancellationToken,
    ) -> TaskResult<Infallible, Error> {
        let (debounce_tx, mut debounce_rx) = mpsc::channel::<EventPayload>(100);

        let _debouncer: JoinHandle<()> = {
            let sender = sender.clone();
            let cancellation_token = cancellation_token.clone();
            tokio::spawn(async move {
                let debouncer = debounce::Debouncer::new(debounce_time);
                loop {
                    let payload = debouncer
                        .read_from_channel(&mut debounce_rx, &cancellation_token)
                        .await;

                    let payload = match payload {
                        debounce::ReadResult::Ok(payload) => payload,
                        #[expect(clippy::panic, reason = "programming error")]
                        debounce::ReadResult::Closed => panic!("debouncer channel closed"),
                        debounce::ReadResult::Cancelled => break,
                    };

                    sender.send(Ok(payload)).await.expect("sender closed");
                }
            })
        };

        loop {
            // Note that `Response::read` is *not* cancellation safe. But in case of shutdown,
            // we do not care about remaining data in the receive buffer.
            let response = tokio::select! {
                response = Response::read(&mut self.connection) => match response {
                    Ok(response) => response,
                    Err(err) => return Err(err).into(),
                },
                () = cancellation_token.cancelled() => {
                    return TaskResult::Cancelled;
                }
            };

            match response {
                Response::Version(_)
                | Response::Workspaces(_)
                | Response::Command(_)
                | Response::Outputs(_)
                | Response::Subscription(_) => sender
                    .send(Err(Error::UnexpectedResponse {
                        expected: ResponseType::SubscriptionEvent,
                        received: response.into(),
                    }))
                    .await
                    .expect("channel open"),
                Response::SubscriptionEvent(event_payload) => {
                    let tx = debounce_tx.clone();
                    tx.send(event_payload)
                        .await
                        .expect("debounce channel closed");
                }
            }
        }
    }
}
