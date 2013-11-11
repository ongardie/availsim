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

#[deriving(Eq, Ord)]
struct Term(uint);

#[deriving(Ord)]
struct Index(uint);

#[deriving(Ord)]
struct Time(uint);

struct Environment {
    clock: Time,
    network: ~[~Message],
}

impl Environment {
    fn new() -> Environment {
        Environment {
            clock: Time(0),
            network: ~[],
        }
    }
    fn make_time(&self, min: uint, max: uint) -> Time {
        Time(*self.clock + randrange(min, max))
    }
    fn make_network_latency(&self, from: ServerID, to: ServerID) -> Time {
        Time(*self.clock + randrange(2, 5))
    }
    fn multicast(&mut self, from: ServerID, to: &[ServerID], body: &MessageBody) {
        for peer in to.iter() {
            let m = ~Message {
                from: from,
                to: *peer,
                deliver: self.make_network_latency(from, *peer),
                body: *body,
            };
            self.network.push(m);
        }
    }
    fn reply(&mut self, request: &Message, response_body: &MessageBody) {
        let m = ~Message {
            from: request.to,
            to: request.from,
            deliver: self.make_network_latency(request.to, request.from),
            body: *response_body,
        };
        self.network.push(m);
    }
    fn pop_ready_messages(&mut self) -> ~[~Message] {
        let mut all = ~[];
        std::util::swap(&mut all, &mut self.network);
        let (ready, notready) = all.partition(|msg| msg.deliver <= self.clock);
        self.network = notready;
        return ready;
    }
}


struct Message {
    from: ServerID,
    to: ServerID,
    deliver: Time,
    body: MessageBody,
}

enum MessageBody {
    RequestVoteRequest {
        term: Term,
        lastLogTerm: Term,
        lastLogIndex: Index,
    },
    RequestVoteResponse {
        term: Term,
        granted: bool,
    },
    Heartbeat {
        term: Term,
    },
}

impl fmt::Default for Message {
    fn fmt(msg: &Message, f: &mut fmt::Formatter) {
        write!(f.buf, "{} to {}: {}",
               *msg.from,
               *msg.to,
               msg.body);
    }
}

impl fmt::Default for MessageBody {
    fn fmt(b: &MessageBody, f: &mut fmt::Formatter) {
        match *b {
            RequestVoteRequest{term, lastLogTerm, lastLogIndex} =>
                write!(f.buf, "RequestVoteRequest({}, {}, {})",
                       term, lastLogTerm, lastLogIndex),
            RequestVoteResponse{term, granted} =>
                write!(f.buf, "RequestVoteResponse({}, {})",
                       term, granted),
            Heartbeat{term} =>
                write!(f.buf, "Heartbeat({})",
                       term),
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
    peers: ~[ServerID],
    term: Term,
    state: ServerState,
    vote: Option<ServerID>,
    log: Log,
}

impl Server {
    fn new(id : ServerID, peers: ~[ServerID], env: &Environment) -> Server {
        Server {
            id: id,
            peers: peers,
            term: Term(0),
            state: Follower { timer: env.make_time(150, 300) },
            log: Log::new(),
            vote: None,
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
                    self.vote = None;
                    self.state = Candidate {
                        timer: env.make_time(150, 300),
                        votes: 1,
                    };
                    env.multicast(self.id, self.peers, &RequestVoteRequest {
                        term: self.term,
                        lastLogTerm: Term(0),
                        lastLogIndex: Index(0),
                    });
                }
                self.try_become_leader(env);
            },
            Leader => {},
        }
    }
    fn step_down(&mut self, env: &Environment, term: Term) {
        self.term = term;
        self.vote = None;
        self.state = Follower {
            timer: env.make_time(150, 300),
        };
    }

    fn try_become_leader(&mut self, env: &mut Environment) {
        match self.state {
            Follower {_} => {},
            Candidate {votes, _} => {
                if votes > (self.peers.len() + 1) / 2 {
                    self.state = Leader;
                    env.multicast(self.id, self.peers, &Heartbeat {
                        term: self.term,
                    });
                }
            },
            Leader => {},
        }
    }
    fn handle(&mut self, env: &mut Environment, msg: &Message) {
        match msg.body {
            RequestVoteRequest {term, _} => {
                let reply = |granted| {
                    env.reply(msg, &RequestVoteResponse {
                        term: self.term,
                        granted: granted,
                    });
                };
                if term < self.term {
                    reply(false);
                } else {
                    if term > self.term {
                        self.step_down(env, term);
                    }
                    match self.vote {
                        None => {
                            self.vote = Some(msg.from);
                            reply(true);
                        },
                        Some(c) => {
                            reply(c == msg.from);
                        },
                    }
                }
            },
            RequestVoteResponse {term, granted, _} => {
                if term == self.term && granted {
                    match self.state {
                        Follower {_} => {},
                        Candidate {votes: ref mut votes, _} => {
                            *votes += 1;
                        },
                        Leader => {},
                    }
                    self.try_become_leader(env)
                } else if term > self.term {
                    self.step_down(env, term);
                }
            },
            Heartbeat {term} => {
                if term == self.term {
                    self.state = Follower {
                        timer: env.make_time(150, 300),
                    };
                } else if term > self.term {
                    self.step_down(env, term);
                }
            },
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
    fn new(env: &Environment, num_servers: uint) -> Servers {
        let mut servers = ~[];
        for i in range(0, num_servers) {
            let mut peers = ~[];
            for j in range(0, num_servers) {
                if i != j {
                    peers.push(ServerID(j + 1));
                }
            }
            let s = Server::new(ServerID(i + 1), peers, env);
            servers.push(s);
        }
        return Servers(servers);
    }

    fn deliver(&mut self, env: &mut Environment, msg: &Message) {
        self[*msg.to - 1].handle(env, msg);
    }
}


fn main() {
    let args = std::os::args();
    let opts = [
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
    let env = &mut Environment::new();
    let mut servers = Servers::new(env, num_servers);
    loop {
        env.clock = Time(*env.clock + 1);
        for server in servers.mut_iter() {
            server.tick(env);
        }
        for msg in env.pop_ready_messages().move_iter() {
            servers.deliver(env, msg);
        }
        for server in servers.iter() {
        //    println(server.to_str());
        }
        for message in env.network.iter() {
            println!("{}", **message);
        }
        println("");
    }
}

