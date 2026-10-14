#![expect(clippy::unwrap_used, reason = "it's just for testing")]

use std::{convert::Infallible, time::Duration};

use tokio::sync::mpsc;
use tokio_util::sync::CancellationToken;

use super::{
    Command, Conn, Error, EventPayload, EventType, Height, Output, OutputMode, OutputName, Outputs,
    PosX, PosY, Position, RefreshRate, Subscribe, TaskResult, Version, Width, Workspace,
    WorkspaceName, WorkspaceNumber, Workspaces,
};

pub enum MockSetting {
    LaptopOnly,
    MonitorOnly(usize),
    Mixed,
}

pub struct MockConnection {
    pub fail: bool,
    pub setting: MockSetting,
}

impl MockConnection {
    fn check_fail(&self) -> Result<(), Error> {
        if self.fail {
            Err(Error::Connection("fail".into()))
        } else {
            Ok(())
        }
    }
}

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
                name: OutputName::try_new("eDP-1".to_owned()).unwrap(),
                mode: OutputMode {
                    width: Width::new(1920),
                    height: Height::new(1080),
                    refresh_rate: RefreshRate::Ms(144),
                },
                active: true,
                position: Position {
                    x: PosX(0),
                    y: PosY(0),
                },
            }])),
            MockSetting::MonitorOnly(num) => match num {
                1 => Ok(Outputs(vec![Output {
                    name: OutputName::try_new("DP-1".to_owned()).unwrap(),
                    mode: OutputMode {
                        width: Width::new(1920),
                        height: Height::new(1080),
                        refresh_rate: RefreshRate::Ms(144),
                    },
                    active: true,
                    position: Position {
                        x: PosX(0),
                        y: PosY(0),
                    },
                }])),
                2 => Ok(Outputs(vec![
                    Output {
                        name: OutputName::try_new("DP-1".to_owned()).unwrap(),
                        mode: OutputMode {
                            width: Width::new(1920),
                            height: Height::new(1080),
                            refresh_rate: RefreshRate::Ms(144),
                        },
                        active: true,
                        position: Position {
                            x: PosX(0),
                            y: PosY(0),
                        },
                    },
                    Output {
                        name: OutputName::try_new("DP-2".to_owned()).unwrap(),
                        mode: OutputMode {
                            width: Width::new(1024),
                            height: Height::new(800),
                            refresh_rate: RefreshRate::Ms(144),
                        },
                        active: true,
                        position: Position {
                            x: PosX(0),
                            y: PosY(0),
                        },
                    },
                ])),
                #[expect(clippy::panic, reason = "just a mock")]
                _ => panic!(),
            },
            MockSetting::Mixed => Ok(Outputs(vec![
                Output {
                    name: OutputName::try_new("eDP-1".to_owned()).unwrap(),
                    mode: OutputMode {
                        width: Width::new(1920),
                        height: Height::new(1080),
                        refresh_rate: RefreshRate::Ms(144),
                    },
                    active: true,
                    position: Position {
                        x: PosX(0),
                        y: PosY(0),
                    },
                },
                Output {
                    name: OutputName::try_new("HDMI-1".to_owned()).unwrap(),
                    mode: OutputMode {
                        width: Width::new(2560),
                        height: Height::new(1440),
                        refresh_rate: RefreshRate::Ms(144),
                    },
                    active: true,
                    position: Position {
                        x: PosX(0),
                        y: PosY(0),
                    },
                },
                Output {
                    name: OutputName::try_new("DP-1".to_owned()).unwrap(),
                    mode: OutputMode {
                        width: Width::new(1024),
                        height: Height::new(800),
                        refresh_rate: RefreshRate::Ms(60),
                    },
                    active: true,
                    position: Position {
                        x: PosX(0),
                        y: PosY(0),
                    },
                },
            ])),
        }
    }

    async fn workspaces(&mut self) -> Result<Workspaces, Error> {
        self.check_fail()?;
        match self.setting {
            MockSetting::LaptopOnly => Ok(Workspaces(vec![
                Workspace {
                    num: WorkspaceNumber::new(1),
                    name: WorkspaceName::try_new("num1".to_owned()).unwrap(),
                    output: OutputName::try_new("eDP-1".to_owned()).unwrap(),
                },
                Workspace {
                    num: WorkspaceNumber::new(2),
                    name: WorkspaceName::try_new("num2".to_owned()).unwrap(),
                    output: OutputName::try_new("eDP-1".to_owned()).unwrap(),
                },
                Workspace {
                    num: WorkspaceNumber::new(3),
                    name: WorkspaceName::try_new("num3".to_owned()).unwrap(),
                    output: OutputName::try_new("eDP-1".to_owned()).unwrap(),
                },
                Workspace {
                    num: WorkspaceNumber::new(4),
                    name: WorkspaceName::try_new("num4".to_owned()).unwrap(),
                    output: OutputName::try_new("eDP-1".to_owned()).unwrap(),
                },
            ])),
            MockSetting::MonitorOnly(num) => match num {
                1 => Ok(Workspaces(vec![
                    Workspace {
                        num: WorkspaceNumber::new(1),
                        name: WorkspaceName::try_new("num1".to_owned()).unwrap(),
                        output: OutputName::try_new("DP-1".to_owned()).unwrap(),
                    },
                    Workspace {
                        num: WorkspaceNumber::new(2),
                        name: WorkspaceName::try_new("num2".to_owned()).unwrap(),
                        output: OutputName::try_new("DP-1".to_owned()).unwrap(),
                    },
                    Workspace {
                        num: WorkspaceNumber::new(3),
                        name: WorkspaceName::try_new("num3".to_owned()).unwrap(),
                        output: OutputName::try_new("DP-1".to_owned()).unwrap(),
                    },
                    Workspace {
                        num: WorkspaceNumber::new(4),
                        name: WorkspaceName::try_new("num4".to_owned()).unwrap(),
                        output: OutputName::try_new("DP-1".to_owned()).unwrap(),
                    },
                ])),
                2 => Ok(Workspaces(vec![
                    Workspace {
                        num: WorkspaceNumber::new(1),
                        name: WorkspaceName::try_new("num1".to_owned()).unwrap(),
                        output: OutputName::try_new("DP-1".to_owned()).unwrap(),
                    },
                    Workspace {
                        num: WorkspaceNumber::new(2),
                        name: WorkspaceName::try_new("num2".to_owned()).unwrap(),
                        output: OutputName::try_new("DP-1".to_owned()).unwrap(),
                    },
                    Workspace {
                        num: WorkspaceNumber::new(3),
                        name: WorkspaceName::try_new("num3".to_owned()).unwrap(),
                        output: OutputName::try_new("DP-2".to_owned()).unwrap(),
                    },
                    Workspace {
                        num: WorkspaceNumber::new(10),
                        name: WorkspaceName::try_new("num10".to_owned()).unwrap(),
                        output: OutputName::try_new("DP-2".to_owned()).unwrap(),
                    },
                ])),
                #[expect(clippy::panic, reason = "just a mock")]
                _ => panic!(),
            },
            MockSetting::Mixed => Ok(Workspaces(vec![
                Workspace {
                    num: WorkspaceNumber::new(1),
                    name: WorkspaceName::try_new("num1".to_owned()).unwrap(),
                    output: OutputName::try_new("eDP-1".to_owned()).unwrap(),
                },
                Workspace {
                    num: WorkspaceNumber::new(2),
                    name: WorkspaceName::try_new("num2".to_owned()).unwrap(),
                    output: OutputName::try_new("eDP-1".to_owned()).unwrap(),
                },
                Workspace {
                    num: WorkspaceNumber::new(3),
                    name: WorkspaceName::try_new("num3".to_owned()).unwrap(),
                    output: OutputName::try_new("DP-1".to_owned()).unwrap(),
                },
                Workspace {
                    num: WorkspaceNumber::new(10),
                    name: WorkspaceName::try_new("num10".to_owned()).unwrap(),
                    output: OutputName::try_new("HDMI-1".to_owned()).unwrap(),
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

pub struct MockSubscription;

#[expect(clippy::infinite_loop, reason = "it's for testing")]
impl Subscribe for MockSubscription {
    async fn listen(
        &mut self,
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
