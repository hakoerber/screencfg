use tokio::{
    io::{self, AsyncReadExt as _, AsyncWriteExt as _},
    net,
};

use super::{
    Command, Error, EventType, Message, Outputs, Response, ResponseType, Subscribe, Subscription,
    Version, Workspaces,
};

pub trait Conn
where
    Self: Sized,
{
    type Subscription: Subscribe;

    fn version(&mut self) -> impl Future<Output = Result<Version, Error>>;
    fn outputs(&mut self) -> impl Future<Output = Result<Outputs, Error>>;
    fn workspaces(&mut self) -> impl Future<Output = Result<Workspaces, Error>>;
    fn command(&mut self, command: Command<'_>) -> impl Future<Output = Result<(), Error>>;
    fn subscribe(
        self,
        event_types: &[EventType],
    ) -> impl Future<Output = Result<Self::Subscription, Error>> + Send;
}

pub struct Connection(io::BufReader<net::UnixStream>);

impl Connection {
    pub(crate) fn new(from: io::BufReader<net::UnixStream>) -> Self {
        Self(from)
    }

    pub(crate) async fn write_all(&mut self, data: &[u8]) -> io::Result<()> {
        self.0.write_all(data).await
    }

    pub(crate) async fn read_exact(&mut self, data: &mut [u8]) -> io::Result<usize> {
        self.0.read_exact(data).await
    }
}

impl Conn for Connection {
    type Subscription = Subscription;

    async fn version(&mut self) -> Result<Version, Error> {
        Message::Version.send(self).await?;

        let response = Response::read(self).await?;

        match response {
            Response::Version(version) => Ok(version.into()),
            Response::Workspaces(_)
            | Response::Command(_)
            | Response::Outputs(_)
            | Response::Subscription(_)
            | Response::SubscriptionEvent(_) => Err(Error::UnexpectedResponse {
                expected: ResponseType::Version,
                received: response.into(),
            }),
        }
    }

    async fn outputs(&mut self) -> Result<Outputs, Error> {
        Message::Outputs.send(self).await?;
        let response = Response::read(self).await?;

        match response {
            Response::Outputs(outputs) => Ok(outputs.try_into()?),
            Response::Version(_)
            | Response::Workspaces(_)
            | Response::Command(_)
            | Response::Subscription(_)
            | Response::SubscriptionEvent(_) => Err(Error::UnexpectedResponse {
                expected: ResponseType::Outputs,
                received: response.into(),
            }),
        }
    }

    async fn workspaces(&mut self) -> Result<Workspaces, Error> {
        Message::Workspaces.send(self).await?;

        let response = Response::read(self).await?;

        match response {
            Response::Workspaces(workspaces) => Ok(workspaces.try_into()?),
            Response::Version(_)
            | Response::Command(_)
            | Response::Outputs(_)
            | Response::Subscription(_)
            | Response::SubscriptionEvent(_) => Err(Error::UnexpectedResponse {
                expected: ResponseType::Workspaces,
                received: response.into(),
            }),
        }
    }

    async fn command(&mut self, command: Command<'_>) -> Result<(), Error> {
        Message::Command(command).send(self).await?;

        let response = Response::read(self).await?;
        match response {
            Response::Command(commands) => {
                for payload in commands {
                    if !payload.success {
                        return Err(Error::Command(
                            payload
                                .error
                                .map_or_else(|| "unknown error".into(), Into::into),
                        ));
                    }
                }
                Ok(())
            }
            Response::Version(_)
            | Response::Workspaces(_)
            | Response::Outputs(_)
            | Response::Subscription(_)
            | Response::SubscriptionEvent(_) => Err(Error::UnexpectedResponse {
                expected: ResponseType::Command,
                received: response.into(),
            }),
        }
    }

    async fn subscribe(mut self, event_types: &[EventType]) -> Result<Subscription, Error> {
        Message::Subscribe(event_types).send(&mut self).await?;

        let response = Response::read(&mut self).await?;

        match response {
            Response::Version(_)
            | Response::Workspaces(_)
            | Response::Command(_)
            | Response::Outputs(_)
            | Response::SubscriptionEvent(_) => {
                return Err(Error::UnexpectedResponse {
                    expected: ResponseType::Subscription,
                    received: response.into(),
                });
            }
            Response::Subscription(subscription_response) => {
                if !subscription_response.success {
                    return Err(Error::ErrorResponse {
                        response_type: ResponseType::Subscription,
                        msg: match subscription_response.error {
                            Some(message) => message.into(),
                            None => "no error message".into(),
                        },
                    });
                }
            }
        }

        Ok(Subscription::new(self))
    }
}
