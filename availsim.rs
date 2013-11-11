 #[feature(struct_variant)];
extern mod extra;
use extra::getopts;
use std::rand;
use std::fmt;

fn randrange(min : uint, max : uint) -> uint {
    min + rand::random::<uint>() % (max - min)
}

#[deriving(Eq)]
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

impl fmt::Default for Message {
    fn fmt(msg: &Message, f: &mut fmt::Formatter) {
        write!(f.buf, "{} to {} in {}: {}",
               *msg.from,
               *msg.to,
               *msg.term,
               msg.body);
    }
}

impl fmt::Default for MessageBody {
    fn fmt(b: &MessageBody, f: &mut fmt::Formatter) {
        match *b {
            RequestVoteRequest{lastLogTerm, lastLogIndex} =>
                write!(f.buf, "RequestVoteRequest({}, {})",
                       lastLogTerm, lastLogIndex),
            RequestVoteResponse{granted} =>
                write!(f.buf, "RequestVoteResponse({})",
                       granted),
        }
    }
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

impl fmt::Default for Term {
    fn fmt(term: &Term, f: &mut fmt::Formatter) {
        write!(f.buf, "{}", **term)
    }
}

impl fmt::Default for Index {
    fn fmt(index: &Index, f: &mut fmt::Formatter) {
        write!(f.buf, "{}", **index)
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
    id: ServerID,
    term: Term,
    state: ServerState,
    log: Log,
}

impl Server {
    fn new(id : ServerID, env: &Environment) -> Server {
        Server {
            id: id,
            term: Term(0),
            state: Follower { timer: env.make_time(150, 300) },
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
    fn tick(&mut self, env: &mut Environment) {
        match self.state {
            Follower  {timer, _} |
            Candidate {timer, _} => {
                if timer <= env.clock {
                    self.term = Term(*self.term + 1);
                    self.state = Candidate {
                        timer: env.make_time(150, 300),
                        votes: 1,
                    };
                    for peer in range(1, env.num_servers) {
                        if ServerID(peer) != self.id {
                            env.network.push(Message {
                                from: self.id,
                                to: ServerID(peer),
                                term: self.term,
                                body: RequestVoteRequest {
                                    lastLogTerm: Term(0),
                                    lastLogIndex: Index(0),
                                },
                            });
                        }
                    }
                }
                self.try_become_leader(env);
            },
            Leader => {},
        }
    }
    fn try_become_leader(&mut self, env: &Environment) {
        match self.state {
            Follower {_} => {},
            Candidate {votes, _} => {
                if votes > env.num_servers / 2 {
                    self.state = Leader;
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
    fn new(env: &Environment) -> Servers {
        let mut servers = ~[];
        for i in range(0, env.num_servers) {
            let s = Server::new(ServerID(i + 1), env);
            servers.push(s);
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
    let env = &mut Environment::new(num_servers);
    let mut servers = Servers::new(env);
    loop {
        env.clock = Time(*env.clock + 1);
        for server in servers.mut_iter() {
            server.tick(env);
        }
        for server in servers.iter() {
            println(server.to_str());
        }
        for message in env.network.iter() {
            println!("{}", *message);
        }
        println("");
    }

    env.network.push(Message {
        from: ServerID(1),
        to: ServerID(2),
        term: Term(0),
        body: RequestVoteRequest {
            lastLogTerm: Term(0),
            lastLogIndex: Index(0),
        },
    });

}

