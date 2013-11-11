 #[feature(struct_variant)];
extern mod extra;
use extra::getopts;
use std::rand;
use std::rc::RcMut;
use std::fmt;

fn randrange(min : uint, max : uint) -> uint {
    min + rand::random::<uint>() % (max - min)
}

struct ServerID(uint);

#[deriving(Ord)]
struct Term(uint);

#[deriving(Ord)]
struct Index(uint);

#[deriving(Ord)]
struct Time(uint);

struct Environment {
    clock: Time,
    num_servers: uint,
    network: ~[Message],
}

impl Environment {
    fn new(num_servers: uint) -> Environment {
        Environment {
            clock: Time(0),
            num_servers: num_servers,
            network: ~[],
        }
    }
    fn make_time(&self, min: uint, max: uint) -> Time {
        Time(*self.clock + randrange(min, max))
    }
}


struct Message {
    from: ServerID,
    to: ServerID,
    term: Term,
    body: MessageBody,
}

enum MessageBody {
    RequestVoteRequest {
        lastLogTerm: Term,
        lastLogIndex: Index,
    },
    RequestVoteResponse {
        granted: bool,
    },
}

enum ServerState {
    Follower  { timer: Time },
    Candidate { timer: Time, votes: uint },
    Leader,
}

impl fmt::Default for Time {
    fn fmt(time: &Time, f: &mut fmt::Formatter) {
        write!(f.buf, "{}", **time)
    }
}

impl fmt::Default for ServerState {
    fn fmt(state: &ServerState, f: &mut fmt::Formatter) {
        match *state {
            Follower{timer} => write!(f.buf, "Follower({})", timer),
            Candidate{timer, votes} => write!(f.buf, "Candidate({}, {})", timer, votes),
            Leader => write!(f.buf, "Leader"),
        }
    }
}

struct LogEntry {
    term: Term,
    // command: ~str,
}

struct Log(~[LogEntry]);

impl Log {
    fn new() -> Log {
        Log(~[])
    }
}

struct Server {
    env: RcMut<Environment>,
    id: ServerID,
    term: Term,
    state: ServerState,
    log: Log,
}

impl Server {
    fn new(id : ServerID, env: &RcMut<Environment>) -> Server {
        Server {
            env: env.clone(),
            id: id,
            term: Term(0),
            state: Follower { timer: env.with_borrow(|e| e.make_time(150, 300)) },
            log: Log::new(),
        }
    }
    /*
    fn reset_timer(&mut self) {
        self.set_timer(Time(*self.env.clock + randrange(150, 300)));
    }
    fn set_timer(&mut self, time: Time) {
        match self.state {
            Follower  {timer : ref mut timer, _} |
            Candidate {timer : ref mut timer, _} =>
                { *timer = time },
            Leader => {},
        }
    }
    */
    fn tick(&mut self) {
        match self.state {
            Follower  {timer, _} |
            Candidate {timer, _} => {
                do self.env.with_borrow |env| {
                    if timer <= env.clock {
                        self.term = Term(*self.term + 1);
                        self.state = Candidate {
                            timer: env.make_time(150, 300),
                            votes: 1,
                        };
                    }
                }
            },
            Leader => {},
        }
    }
    fn try_become_leader(&mut self) {
        match self.state {
            Follower {_} => {},
            Candidate {votes, _} => {
                do self.env.with_borrow |env| {
                    if votes > env.num_servers / 2 {
                        self.state = Leader;
                    }
                }
            },
            Leader => {},
        }
    }
    /*
    fn next_event_time(&self) -> Time {
        match self.state {
            Follower  {timer, _} |
            Candidate {timer, _} =>
                timer,
            Leader => Time(!0)
        }
    }
    */
}

impl ToStr for Server {
    fn to_str(&self) -> ~str {
        format!("Server {}: {}", *self.id, self.state)
    }
}

struct Servers(~[Server]);

impl Servers {
    fn new(env: &RcMut<Environment>) -> Servers {
        let mut servers = ~[];
        do env.with_borrow |e| {
            for i in range(0, e.num_servers) {
                let s = Server::new(ServerID(i + 1), env);
                servers.push(s);
            }
        }
        return Servers(servers);
    }
}


fn main() {
    let args = std::os::args();
    let opts = ~[
        getopts::optflag("h"),
        getopts::optflag("help"),
        getopts::optopt("servers"),
    ];
    let matches = match getopts::getopts(args.tail(), opts) {
        Ok(m) => { m },
        Err(f) => { fail!(f.to_err_msg()) },
    };
    let usage = || {
        println!("Usage: {} [options]", args[0]);
        println("-h, --help   Print this help message");
        println("--servers=N  Simulate cluster with N servers");
    };
    if matches.opt_present("h") || matches.opt_present("help") {
        usage();
        return;
    }
    if !matches.free.is_empty() {
        usage();
        fail!(format!("Extra arguments: {}", matches.free.len()));
    }
    let num_servers : uint = match matches.opt_str("servers") {
        Some(s) => { match std::from_str::from_str(s) {
            Some(i) => { i },
            None => {
                usage();
                fail!(format!("Couldn't parse number of servers from '{}'", s));
            },
        }},
        None => { 5 },
    };
    let env = RcMut::new(Environment::new(num_servers));
    let mut servers = Servers::new(&env);
    loop {
        for server in servers.iter() {
            println(server.to_str());
        }
        do env.with_mut_borrow |e| {
            e.clock = Time(*e.clock + 1);
        }
        for server in servers.mut_iter() {
            server.tick();
        }
    }

    do env.with_mut_borrow |e| {
        e.network.push(Message {
            from: ServerID(1),
            to: ServerID(2),
            term: Term(0),
            body: RequestVoteRequest {
                lastLogTerm: Term(0),
                lastLogIndex: Index(0),
            },
        });
    }
    do env.with_borrow |e| {
        for message in e.network.iter() {
            println(format!("{} to {}", *message.from, *message.to));
        }
    }
}

