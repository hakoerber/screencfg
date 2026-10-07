#![expect(clippy::unwrap_used, reason = "it's just for testing")]

use std::{convert::Infallible, time::Duration};

use tokio::sync::mpsc;
use tokio_util::sync::CancellationToken;

use super::{
    Command, Conn, Error, EventPayload, EventType, Output, OutputName, Outputs, Subscribe,
    TaskResult, Version, Workspace, WorkspaceName, WorkspaceNumber, Workspaces,
};

pub enum MockSetting {
    LaptopOnly,
    ExternalOnly(usize),
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
                active: true,
                primary: true,
            }])),
            MockSetting::ExternalOnly(num) => match num {
                1 => Ok(Outputs(vec![Output {
                    name: OutputName::try_new("DP-1".to_owned()).unwrap(),
                    active: true,
                    primary: false,
                }])),
                2 => Ok(Outputs(vec![
                    Output {
                        name: OutputName::try_new("DP-1".to_owned()).unwrap(),
                        active: true,
                        primary: false,
                    },
                    Output {
                        name: OutputName::try_new("DP-2".to_owned()).unwrap(),
                        active: false,
                        primary: false,
                    },
                ])),
                #[expect(clippy::panic, reason = "just a mock")]
                _ => panic!(),
            },
            MockSetting::Mixed => Ok(Outputs(vec![
                Output {
                    name: OutputName::try_new("eDP-1".to_owned()).unwrap(),
                    active: true,
                    primary: true,
                },
                Output {
                    name: OutputName::try_new("HDMI-1".to_owned()).unwrap(),
                    active: true,
                    primary: false,
                },
                Output {
                    name: OutputName::try_new("DP-1".to_owned()).unwrap(),
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
                    num: WorkspaceNumber::new(1),
                    name: WorkspaceName::try_new("num1".to_owned()).unwrap(),
                    output: OutputName::try_new("eDP-1".to_owned()).unwrap(),
                },
                Workspace {
                    num: WorkspaceNumber::new(1),
                    name: WorkspaceName::try_new("num1".to_owned()).unwrap(),
                    output: OutputName::try_new("eDP-1".to_owned()).unwrap(),
                },
                Workspace {
                    num: WorkspaceNumber::new(1),
                    name: WorkspaceName::try_new("num1".to_owned()).unwrap(),
                    output: OutputName::try_new("eDP-1".to_owned()).unwrap(),
                },
                Workspace {
                    num: WorkspaceNumber::new(1),
                    name: WorkspaceName::try_new("num1".to_owned()).unwrap(),
                    output: OutputName::try_new("eDP-1".to_owned()).unwrap(),
                },
            ])),
            MockSetting::ExternalOnly(num) => match num {
                1 => Ok(Workspaces(vec![
                    Workspace {
                        num: WorkspaceNumber::new(1),
                        name: WorkspaceName::try_new("num1".to_owned()).unwrap(),
                        output: OutputName::try_new("DP-1".to_owned()).unwrap(),
                    },
                    Workspace {
                        num: WorkspaceNumber::new(1),
                        name: WorkspaceName::try_new("num1".to_owned()).unwrap(),
                        output: OutputName::try_new("DP-1".to_owned()).unwrap(),
                    },
                    Workspace {
                        num: WorkspaceNumber::new(1),
                        name: WorkspaceName::try_new("num1".to_owned()).unwrap(),
                        output: OutputName::try_new("DP-1".to_owned()).unwrap(),
                    },
                    Workspace {
                        num: WorkspaceNumber::new(1),
                        name: WorkspaceName::try_new("num1".to_owned()).unwrap(),
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
                        num: WorkspaceNumber::new(1),
                        name: WorkspaceName::try_new("num1".to_owned()).unwrap(),
                        output: OutputName::try_new("DP-1".to_owned()).unwrap(),
                    },
                    Workspace {
                        num: WorkspaceNumber::new(1),
                        name: WorkspaceName::try_new("num1".to_owned()).unwrap(),
                        output: OutputName::try_new("DP-2".to_owned()).unwrap(),
                    },
                    Workspace {
                        num: WorkspaceNumber::new(1),
                        name: WorkspaceName::try_new("num1".to_owned()).unwrap(),
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
                    num: WorkspaceNumber::new(1),
                    name: WorkspaceName::try_new("num1".to_owned()).unwrap(),
                    output: OutputName::try_new("eDP-1".to_owned()).unwrap(),
                },
                Workspace {
                    num: WorkspaceNumber::new(1),
                    name: WorkspaceName::try_new("num1".to_owned()).unwrap(),
                    output: OutputName::try_new("DP-1".to_owned()).unwrap(),
                },
                Workspace {
                    num: WorkspaceNumber::new(1),
                    name: WorkspaceName::try_new("num1".to_owned()).unwrap(),
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
