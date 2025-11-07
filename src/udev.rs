use std::{fmt, io, sync::mpsc, thread, time};

pub(crate) use mio::Token;
pub(crate) use udev::EventType;

#[derive(Debug)]
pub(crate) struct Error(io::Error);

impl From<io::Error> for Error {
    fn from(value: io::Error) -> Self {
        Self(value)
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

// wrap udev::Event to implement `PartialEq`, as this is required for `debounce::EventDebouncer`.
// All events are considered equal for deboucing.
pub(crate) struct Event(udev::Event);

impl PartialEq for Event {
    fn eq(&self, _other: &Self) -> bool {
        true
    }
}

impl fmt::Debug for Event {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Event")
            .field("sequence_number", &self.0.sequence_number())
            .field("type", &self.0.event_type())
            .field("syspath", &self.0.syspath().to_str().unwrap_or("---"))
            .field(
                "subsystem",
                &self.0.subsystem().map_or("", |s| s.to_str().unwrap_or("")),
            )
            .field("sysname", &self.0.sysname().to_str().unwrap_or(""))
            .field(
                "devtype",
                &self.0.devtype().map_or("", |s| s.to_str().unwrap_or("")),
            )
            .finish()
    }
}

pub(crate) struct EventListener {
    socket: udev::MonitorSocket,
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum Subsystem {
    Drm,
}

impl Subsystem {
    fn as_str(self) -> &'static str {
        match self {
            Subsystem::Drm => "drm",
        }
    }
}

impl EventListener {
    pub(crate) fn new(subsystem: Subsystem) -> Result<Self, Error> {
        Ok(Self {
            socket: udev::MonitorBuilder::new()?
                .match_subsystem(subsystem.as_str())?
                .listen()?,
        })
    }
}

pub(crate) struct EventStream {
    listener: EventListener,
    poll: mio::Poll,
    events: mio::Events,
    token: mio::Token,
}

impl EventStream {
    pub(crate) fn from_listener(mut listener: EventListener, token: Token) -> Result<Self, Error> {
        let poll = mio::Poll::new()?;
        poll.registry().register(
            &mut listener.socket,
            token,
            mio::Interest::READABLE | mio::Interest::WRITABLE,
        )?;
        Ok(Self {
            listener,
            poll,
            events: mio::Events::with_capacity(1024),
            token,
        })
    }

    pub(crate) fn handle<'a, F, H, E>(
        &'a mut self,
        filter: F,
        handler: H,
        debounce_duration: time::Duration,
    ) -> E
    where
        F: Fn(udev::EventType) -> bool + Send + 'a,
        H: Fn(Event) -> Result<(), E> + Send + 'static,
        E: Send + 'static,
    {
        thread::scope(|scope| {
            let (event_tx, event_rx) = mpsc::channel::<Event>();
            let (result_tx, result_rx) = mpsc::channel::<E>();

            let _event_handler = scope.spawn(move || {
                // when we receive an event, we bundle the event of the next second and only
                // then trigger the configuration.
                let debouncer = debounce::EventDebouncer::new(debounce_duration, move |event| {
                    if let Err(err) = handler(event) {
                        result_tx.send(err).unwrap();
                    }
                });
                for event in event_rx {
                    debouncer.put(event);
                }
            });

            let _event_producer = scope.spawn(move || {
                loop {
                    if let Err(err) = self.poll.poll(&mut self.events, None).map_err(Error) {
                        return err;
                    }

                    for event in &self.events {
                        if event.token() == self.token && event.is_writable() {
                            for event in self.listener.socket.iter() {
                                if filter(event.event_type()) {
                                    event_tx.send(Event(event)).unwrap();
                                }
                            }
                        }
                    }
                }
            });

            result_rx.recv().unwrap()
        })
    }
}
