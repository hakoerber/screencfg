use std::{
    borrow::Cow,
    convert::Infallible,
    ffi::OsStr,
    fmt,
    ops::{Deref, DerefMut, Index},
    os::unix::{ffi::OsStrExt as _, net::SocketAddr},
    path::PathBuf,
    process,
    time::Duration,
    vec::IntoIter,
};

use serde::{Deserialize, Serialize};
use tokio::{
    io::{self, AsyncReadExt as _, AsyncWriteExt as _},
    net,
    sync::mpsc,
    time,
};
use tokio_util::sync::CancellationToken;

enum ReadResult<T, E> {
    Ok(T),
    Err(E),
    Closed,
    Cancelled,
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

mod error;
pub use error::Error;

#[derive(Debug, Clone, Copy)]
pub struct WorkspaceNumber(usize);

impl WorkspaceNumber {
    pub fn new(value: usize) -> Self {
        Self(value)
    }

    pub fn into_inner(self) -> usize {
        self.0
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct WorkspaceName(String);

impl WorkspaceName {
    pub fn into_inner(self) -> String {
        self.0
    }
}

impl fmt::Display for WorkspaceNumber {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug)]
pub struct OutputName(String);

impl OutputName {
    pub fn new(value: String) -> Self {
        Self(value)
    }

    pub fn into_string(self) -> String {
        self.0
    }
}

impl fmt::Display for OutputName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl From<String> for OutputName {
    fn from(value: String) -> Self {
        Self(value)
    }
}

pub struct OutputNameRef<'a>(&'a String);

impl<'a> OutputNameRef<'a> {
    pub fn new(value: &'a String) -> Self {
        Self(value)
    }
}

impl fmt::Display for OutputNameRef<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

pub enum Command<'out> {
    Nop,
    MoveWorkspace {
        number: WorkspaceNumber,
        output_name: OutputNameRef<'out>,
    },
    Reload,
}

impl From<&Command<'_>> for Cow<'static, str> {
    fn from(value: &Command<'_>) -> Self {
        match *value {
            Command::Nop => Cow::from("nop"),
            Command::MoveWorkspace {
                number: id,
                ref output_name,
            } => Cow::from(format!(
                "[workspace=\"{id}\"] move workspace to output {output_name}"
            )),
            Command::Reload => Cow::from("reload"),
        }
    }
}

pub trait Conn {
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

#[cfg(any(test, feature = "testing"))]
pub enum MockSetting {
    LaptopOnly,
    ExternalOnly(usize),
    Mixed,
}

#[cfg(any(test, feature = "testing"))]
pub struct MockConnection {
    pub fail: bool,
    pub setting: MockSetting,
}

#[cfg(any(test, feature = "testing"))]
impl MockConnection {
    fn check_fail(&self) -> Result<(), Error> {
        if self.fail {
            Err(Error::Connection("fail".into()))
        } else {
            Ok(())
        }
    }
}

#[cfg(any(test, feature = "testing"))]
#[expect(
    clippy::unused_async_trait_impl,
    reason = "true, but it's just for testing"
)]
impl Conn for MockConnection {
    type Subscription = MockSubscription;

    async fn version(&mut self) -> Result<Version, Error> {
        self.check_fail()?;
        Ok(Version {
            minor: 1,
            patch: 2,
            major: 3,
        })
    }

    async fn outputs(&mut self) -> Result<Outputs, Error> {
        self.check_fail()?;
        match self.setting {
            MockSetting::LaptopOnly => Ok(Outputs(vec![Output {
                name: OutputName::new("eDP-1".into()),
                active: true,
                primary: true,
            }])),
            MockSetting::ExternalOnly(num) => match num {
                1 => Ok(Outputs(vec![Output {
                    name: OutputName::new("DP-1".into()),
                    active: true,
                    primary: false,
                }])),
                2 => Ok(Outputs(vec![
                    Output {
                        name: OutputName::new("DP-1".into()),
                        active: true,
                        primary: false,
                    },
                    Output {
                        name: OutputName::new("DP-2".into()),
                        active: false,
                        primary: false,
                    },
                ])),
                #[expect(clippy::panic, reason = "just a mock")]
                _ => panic!(),
            },
            MockSetting::Mixed => Ok(Outputs(vec![
                Output {
                    name: OutputName::new("eDP-1".into()),
                    active: true,
                    primary: true,
                },
                Output {
                    name: OutputName::new("HDMI-1".into()),
                    active: true,
                    primary: false,
                },
                Output {
                    name: OutputName::new("DP-1".into()),
                    active: true,
                    primary: false,
                },
            ])),
        }
    }

    async fn workspaces(&mut self) -> Result<Workspaces, Error> {
        self.check_fail()?;
        match self.setting {
            MockSetting::LaptopOnly => Ok(Workspaces(vec![
                Workspace {
                    num: WorkspaceNumber(1),
                    name: WorkspaceName("num1".into()),
                    output: OutputName("eDP-1".into()),
                },
                Workspace {
                    num: WorkspaceNumber(1),
                    name: WorkspaceName("num1".into()),
                    output: OutputName("eDP-1".into()),
                },
                Workspace {
                    num: WorkspaceNumber(1),
                    name: WorkspaceName("num1".into()),
                    output: OutputName("eDP-1".into()),
                },
                Workspace {
                    num: WorkspaceNumber(1),
                    name: WorkspaceName("num1".into()),
                    output: OutputName("eDP-1".into()),
                },
            ])),
            MockSetting::ExternalOnly(num) => match num {
                1 => Ok(Workspaces(vec![
                    Workspace {
                        num: WorkspaceNumber(1),
                        name: WorkspaceName("num1".into()),
                        output: OutputName("DP-1".into()),
                    },
                    Workspace {
                        num: WorkspaceNumber(1),
                        name: WorkspaceName("num1".into()),
                        output: OutputName("DP-1".into()),
                    },
                    Workspace {
                        num: WorkspaceNumber(1),
                        name: WorkspaceName("num1".into()),
                        output: OutputName("DP-1".into()),
                    },
                    Workspace {
                        num: WorkspaceNumber(1),
                        name: WorkspaceName("num1".into()),
                        output: OutputName("DP-1".into()),
                    },
                ])),
                2 => Ok(Workspaces(vec![
                    Workspace {
                        num: WorkspaceNumber(1),
                        name: WorkspaceName("num1".into()),
                        output: OutputName("DP-1".into()),
                    },
                    Workspace {
                        num: WorkspaceNumber(1),
                        name: WorkspaceName("num1".into()),
                        output: OutputName("DP-1".into()),
                    },
                    Workspace {
                        num: WorkspaceNumber(1),
                        name: WorkspaceName("num1".into()),
                        output: OutputName("DP-2".into()),
                    },
                    Workspace {
                        num: WorkspaceNumber(1),
                        name: WorkspaceName("num1".into()),
                        output: OutputName("DP-2".into()),
                    },
                ])),
                #[expect(clippy::panic, reason = "just a mock")]
                _ => panic!(),
            },
            MockSetting::Mixed => Ok(Workspaces(vec![
                Workspace {
                    num: WorkspaceNumber(1),
                    name: WorkspaceName("num1".into()),
                    output: OutputName("eDP-1".into()),
                },
                Workspace {
                    num: WorkspaceNumber(1),
                    name: WorkspaceName("num1".into()),
                    output: OutputName("eDP-1".into()),
                },
                Workspace {
                    num: WorkspaceNumber(1),
                    name: WorkspaceName("num1".into()),
                    output: OutputName("DP-1".into()),
                },
                Workspace {
                    num: WorkspaceNumber(1),
                    name: WorkspaceName("num1".into()),
                    output: OutputName("HDMI-1".into()),
                },
            ])),
        }
    }

    async fn command(&mut self, _command: Command<'_>) -> Result<(), Error> {
        self.check_fail()?;
        Ok(())
    }

    async fn subscribe(self, _event_types: &[EventType]) -> Result<MockSubscription, Error> {
        Ok(MockSubscription {})
    }
}

#[cfg(any(test, feature = "testing"))]
pub struct MockSubscription;

#[cfg(any(test, feature = "testing"))]
#[expect(clippy::infinite_loop, reason = "it's for testing")]
impl Subscribe for MockSubscription {
    async fn listen(
        self,
        sender: mpsc::Sender<Result<EventPayload, Error>>,
        debounce_time: Duration,
        _cancellation_token: CancellationToken,
    ) -> TaskResult<Infallible, Error> {
        loop {
            use std::thread;

            sender
                .send(Ok(EventPayload::Output))
                .await
                .expect("channel open");
            thread::sleep(debounce_time);
        }
    }
}

pub struct Connection(io::BufReader<net::UnixStream>);

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
            Response::Outputs(outputs) => Ok(outputs.into()),
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
            Response::Workspaces(workspaces) => Ok(workspaces.into()),
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

        Ok(Subscription { connection: self })
    }
}

pub struct Cancelled;

pub trait Subscribe
where
    Self: Sized,
{
    fn listen(
        self,
        sender: mpsc::Sender<Result<EventPayload, Error>>,
        debounce_time: Duration,
        cancellation_token: CancellationToken,
    ) -> impl Future<Output = TaskResult<Infallible, Error>>;
}

pub struct Subscription {
    connection: Connection,
}

/// * If the channel gets closed after we received an event, but before the event is handled,
///   we will **not** run `f()`.
///
/// * `f()` will be called with the **first** received value in the debounce duration. All other
///   values will be discarded
async fn read_from_channel_with_debounce<V, F, T, E>(
    mut channel: mpsc::Receiver<V>,
    f: F,
    duration: Duration,
    cancellation_token: CancellationToken,
) -> ReadResult<T, Error>
where
    F: AsyncFn(V) -> Result<T, Error>,
{
    let sleep = time::sleep(duration);
    tokio::pin!(sleep);

    match channel.recv().await {
        Some(v) => loop {
            tokio::select! {
                _ = &mut sleep => {
                    return match f(v).await {
                        Ok(v) => ReadResult::Ok(v),
                        Err(err) => ReadResult::Err(err),

                    };
                },
                v = channel.recv() => match v {
                    Some(v) => drop(v),
                    None => return ReadResult::Closed,

                },
                () = cancellation_token.cancelled() => {
                    return ReadResult::Cancelled;
                }
            }
        },
        None => return ReadResult::Closed,
    }
    // tokio::select! {
    //     v = channel.recv() => {

    //     },
    //     () = time::sleep()
    // }
}

impl Subscribe for Subscription {
    async fn listen(
        mut self,
        sender: mpsc::Sender<Result<EventPayload, Error>>,
        debounce_time: Duration,
        cancellation_token: CancellationToken,
    ) -> TaskResult<Infallible, Error> {
        let (tx, rx) = mpsc::channel::<EventPayload>(100);

        let debouncer = {
            let sender = sender.clone();
            let cancellation_token = cancellation_token.clone();
            tokio::spawn(async move {
                read_from_channel_with_debounce::<_, _, _, Error>(
                    rx,
                    async move |event| Ok(sender.send(Ok(event)).await.unwrap()),
                    debounce_time,
                    cancellation_token,
                )
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
                    let tx = tx.clone();
                    tokio::spawn(async move { tx.send(event_payload).await.unwrap() })
                        .await
                        .expect("thread to not panic");
                }
            }
        }
    }
}

fn get_socketpath() -> Result<PathBuf, Error> {
    let cmd = process::Command::new("i3")
        .arg("--get-socketpath")
        .output()?;

    let bytes = cmd
        .stdout
        .into_iter()
        .take_while(|c| *c != b'\n')
        .collect::<Vec<u8>>();

    let string = OsStr::from_bytes(&bytes);

    let path = PathBuf::from(string);

    Ok(path)
}

pub async fn connect() -> Result<Connection, Error> {
    let socketpath = get_socketpath()?;

    let socket = SocketAddr::from_pathname(socketpath)?;

    let stream = net::UnixStream::connect_addr(&socket.into()).await?;

    let buffer = io::BufReader::new(stream);

    Ok(Connection(buffer))
}

/// This takes ownership of connection because event handling
/// should have a separate connection, as it fucks with request ordering
///
/// <https://i3wm.org/docs/ipc.html#_events>
///
/// > As soon as you subscribe to an event, it is not guaranteed any longer
/// > that the requests to i3 are processed in order. This means, the
/// > following situation can happen: You send a GET_WORKSPACES request
/// > but you receive a "workspace" event before receiving the reply to
/// > GET_WORKSPACES. If your program does not want to cope which such kinds
/// > of race conditions (an event based library may not have a problem here),
/// > I suggest you create a separate connection to receive events.
pub async fn start_event_listener<F, E>(
    connection: Connection,
    debounce_time: Duration,
    event_types: &'static [EventType],
    handler: F,
) -> Result<Infallible, E>
where
    F: AsyncFn(EventPayload) -> Result<(), E>,
    E: From<Error>,
    // this can be enabled when async_fn_traits is stabilized:
    //
    // for<'a> F::CallRefFuture<'a>: Send,
{
    let (tx, mut rx) = mpsc::channel(100);
    let cancellation_token = CancellationToken::new();

    let subscription = connection.subscribe(event_types).await?;

    let event_subscriber = {
        let cancellation_token = cancellation_token.clone();
        tokio::spawn(async move {
            subscription
                .listen(tx, debounce_time, cancellation_token)
                .await
        })
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
        (ReadResult::Closed | ReadResult::Cancelled, TaskResult::Cancelled) => {
            panic!("threads finished unexpectedly without an error")
        }
        (ReadResult::Closed | ReadResult::Cancelled, TaskResult::Err(err)) => err.into(),
        (ReadResult::Err(err), TaskResult::Cancelled) => err.into(),
        (ReadResult::Err(_err1), TaskResult::Err(_err2)) => {
            panic!("teardown of subscriber seems to have produced an error")
        }
    })
}

#[derive(Debug, serde::Deserialize)]
struct OutputPayload {
    name: String,
    active: bool,
    primary: bool,
}

#[derive(Debug)]
pub struct Output {
    pub name: OutputName,
    pub active: bool,
    pub primary: bool,
}

impl fmt::Display for Output {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)?;
        if self.active {
            write!(f, " [active]")?;
        }
        if self.primary {
            write!(f, " [primary]")?;
        }
        Ok(())
    }
}

impl From<OutputPayload> for Output {
    fn from(value: OutputPayload) -> Self {
        Self {
            name: value.name.into(),
            active: value.active,
            primary: value.primary,
        }
    }
}

#[derive(Debug)]
pub struct Workspaces(Vec<Workspace>);

impl From<Vec<WorkspacePayload>> for Workspaces {
    fn from(value: Vec<WorkspacePayload>) -> Self {
        Self(value.into_iter().map(Into::into).collect())
    }
}

impl IntoIterator for Workspaces {
    type Item = Workspace;
    type IntoIter = IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl Deref for Workspaces {
    type Target = [Workspace];

    fn deref(&self) -> &[Workspace] {
        &self.0
    }
}
impl DerefMut for Workspaces {
    fn deref_mut(&mut self) -> &mut [Workspace] {
        &mut self.0
    }
}

#[derive(Debug)]
pub struct Outputs(Vec<Output>);

impl From<Vec<OutputPayload>> for Outputs {
    fn from(value: Vec<OutputPayload>) -> Self {
        Self(
            value
                .into_iter()
                .filter(|output| output.name != "xroot-0")
                .map(Into::into)
                .collect(),
        )
    }
}

impl IntoIterator for Outputs {
    type Item = Output;
    type IntoIter = <Vec<Output> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl Deref for Outputs {
    type Target = [Output];

    fn deref(&self) -> &[Output] {
        &self.0
    }
}
impl DerefMut for Outputs {
    fn deref_mut(&mut self) -> &mut [Output] {
        &mut self.0
    }
}

impl Index<usize> for Outputs {
    type Output = Output;

    #[expect(
        clippy::indexing_slicing,
        reason = "transparent slicing, panicking is ok"
    )]
    fn index(&self, index: usize) -> &Self::Output {
        &self.0[index]
    }
}

#[derive(Serialize)]
pub enum EventType {
    #[serde(rename = "output")]
    Output,
    #[serde(rename = "workspace")]
    Workspace,
}

enum Message<'input> {
    Command(Command<'input>),
    Subscribe(&'input [EventType]),
    Workspaces,
    Outputs,
    Version,
}

impl From<Message<'_>> for u32 {
    fn from(value: Message<'_>) -> Self {
        match value {
            Message::Command(_) => 0,
            Message::Subscribe(_) => 2,
            Message::Workspaces => 1,
            Message::Outputs => 3,
            Message::Version => 7,
        }
    }
}

impl Message<'_> {
    fn bytes(self) -> Result<Vec<u8>, Error> {
        let payload: Option<Cow<'static, str>> = match self {
            Self::Command(ref command) => Some(command.into()),
            Self::Subscribe(ref event_types) => Some(
                serde_json::to_string(event_types)
                    .expect("serializing static values always succeeds")
                    .into(),
            ),
            Self::Workspaces | Self::Outputs | Self::Version => None,
        };

        let mut message: Vec<u8> = vec![];
        let command_number: u32 = self.into();

        message.extend_from_slice(b"i3-ipc");
        message.extend_from_slice(
            &u32::try_from(payload.as_ref().map_or(0, |l| l.len()))
                .map_err(|_err| Error::Protocol("payload length bigger than 4 bytes".into()))?
                .to_ne_bytes(),
        );
        message.extend_from_slice(&(command_number.to_ne_bytes()));
        if let Some(payload) = payload {
            message.extend_from_slice(payload.as_bytes());
        }
        Ok(message)
    }

    async fn send(self, socket: &mut Connection) -> Result<(), Error> {
        let message = self.bytes()?;
        socket.0.write_all(&message).await?;
        Ok(())
    }
}

#[derive(Debug, serde::Deserialize)]
#[expect(dead_code, reason = "external data defintion")]
struct VersionPayload {
    human_readable: String,
    loaded_config_file_name: String,
    major: usize,
    minor: usize,
    patch: usize,
}

pub struct Version {
    minor: usize,
    patch: usize,
    major: usize,
}

impl fmt::Display for Version {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}.{}.{}", self.major, self.minor, self.patch)
    }
}

impl From<VersionPayload> for Version {
    fn from(value: VersionPayload) -> Self {
        Self {
            major: value.major,
            minor: value.minor,
            patch: value.patch,
        }
    }
}

#[derive(Debug, serde::Deserialize)]
#[expect(dead_code, reason = "external data defintion")]
struct WorkspacePayload {
    id: usize,
    num: usize,
    name: String,
    output: String,
}

#[derive(Debug, serde::Deserialize)]
struct CommandPayload {
    success: bool,
    error: Option<String>,
}

#[derive(Debug)]
pub struct Workspace {
    pub num: WorkspaceNumber,
    pub name: WorkspaceName,
    pub output: OutputName,
}

impl fmt::Display for Workspace {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} on {}", self.num, self.output)
    }
}

impl From<WorkspacePayload> for Workspace {
    fn from(value: WorkspacePayload) -> Self {
        Self {
            num: WorkspaceNumber(value.num),
            name: WorkspaceName(value.name),
            output: OutputName(value.output),
        }
    }
}

#[derive(Debug, Deserialize, Clone)]
pub struct EventPayloadWorkspace {
    pub change: String,
}

impl fmt::Display for EventPayloadWorkspace {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "change: {}", self.change)
    }
}

#[derive(Debug, serde::Deserialize)]
struct SubscriptionPayload {
    success: bool,
    error: Option<String>,
}

#[derive(Debug, strum::EnumDiscriminants, Clone)]
#[strum_discriminants(derive(Hash), name(EventPayloadType))]
pub enum EventPayload {
    Output,
    Workspace(EventPayloadWorkspace),
}

impl fmt::Display for EventPayload {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Self::Output => write!(f, "Output"),
            Self::Workspace(ref event_payload_workspace) => {
                write!(f, "Workspace({event_payload_workspace})")
            }
        }
    }
}

#[derive(Debug, strum::EnumDiscriminants)]
#[strum_discriminants(derive(strum::Display), vis(pub), name(ResponseType))]
enum Response {
    Version(VersionPayload),
    Workspaces(Vec<WorkspacePayload>),
    Command(Vec<CommandPayload>),
    Outputs(Vec<OutputPayload>),
    Subscription(SubscriptionPayload),
    SubscriptionEvent(EventPayload),
}

impl Response {
    async fn read(stream: &mut Connection) -> Result<Self, Error> {
        let mut response =
            vec![
                0;
                "i3-ipc"
                    .chars()
                    .count()
                    .checked_add(
                        // for response length
                        4 +
                        // for command
                        4
                    )
                    .ok_or_else(|| { Error::Protocol("payload length overflowed".into()) })?
            ];

        let _bytes_count = stream.0.read_exact(&mut response).await?;

        if &response
            .get(0..6)
            .ok_or_else(|| Error::Protocol("response too short for even the magic string".into()))?
            != b"i3-ipc"
        {
            return Err(Error::Protocol("magic string not found".into()));
        }
        let response_length = {
            let bytes = response
                .get(6..10)
                .ok_or_else(|| Error::Protocol("not enough bytes for response length".into()))?;

            let bytes = bytes
                .try_into()
                .expect("slice of length 4 can always be converted into an array of size 4");

            u32::from_ne_bytes(bytes)
        };

        let response_command = {
            let bytes = response
                .get(10..14)
                .ok_or_else(|| Error::Protocol("not enough bytes for command".into()))?;

            let bytes = bytes
                .try_into()
                .expect("slice of length 4 can always be converted into an array of size 4");

            u32::from_ne_bytes(bytes)
        };

        response = vec![
            0;
            response_length
                .try_into()
                .map_err(|_err| { Error::Protocol("u32 overflowed usize".into()) })?
        ];

        let _bytes_count = stream.0.read_exact(&mut response).await?;

        // highest bit indicates event response
        if response_command >> 31_u8 == 1 {
            let response_command = response_command & !(1 << 31_u8);
            Ok(Self::SubscriptionEvent(match response_command {
                0 => EventPayload::Workspace(serde_json::from_slice(&response)?),
                1 => {
                    #[derive(Debug, serde::Deserialize)]
                    struct EventResponseOutput {
                        change: String,
                    }

                    let response: EventResponseOutput = serde_json::from_slice(&response)?;
                    assert_eq!(response.change, "unspecified", "this is a static response");
                    EventPayload::Output
                }
                id => return Err(Error::UnknownResponseCommand { id, event: true }),
            }))
        } else {
            match response_command {
                0 => Ok(Self::Command(serde_json::from_slice(&response)?)),
                1 => Ok(Self::Workspaces(serde_json::from_slice(&response)?)),
                2 => Ok(Self::Subscription(serde_json::from_slice(&response)?)),
                3 => Ok(Self::Outputs(serde_json::from_slice(&response)?)),
                7 => Ok(Self::Version(serde_json::from_slice(&response)?)),
                id => Err(Error::UnknownResponseCommand { id, event: false }),
            }
        }
    }
}
