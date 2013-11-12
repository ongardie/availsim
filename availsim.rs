 #[feature(struct_variant)];
extern mod extra;
use extra::getopts;
use std::rand;
use std::fmt;

fn randrange(min : uint, max : uint) -> uint {
    min + rand::random::<uint>() % (max - min + 1)
}

#[deriving(Eq, IterBytes, Clone)]
struct ServerID(uint);

#[deriving(Eq, Ord)]
struct Term(uint);

impl fmt::Default for Term {
    fn fmt(term: &Term, f: &mut fmt::Formatter) {
        write!(f.buf, "{}", **term)
    }
}


#[deriving(Ord)]
struct Index(uint);

impl fmt::Default for Index {
    fn fmt(index: &Index, f: &mut fmt::Formatter) {
        write!(f.buf, "{}", **index)
    }
}


#[deriving(Ord, Clone)]
struct Time(uint);

// An integer big enough to represent infinity in the simulations, but small
// enough that it'll never overflow.
static NEVER : uint = 1<<30;


impl Add<uint, Time> for Time {
    fn add(&self, rhs: &uint) -> Time {
      Time(**self + *rhs)
  }
}

impl fmt::Default for Time {
    fn fmt(time: &Time, f: &mut fmt::Formatter) {
        write!(f.buf, "{}", **time)
    }
}

struct Environment {
    clock: Time,
    network: ~[~Message],
    timing: ~TimingPolicy,
}

trait TimingPolicy {
    fn network_latency(&self, _from: ServerID, _to: ServerID) -> uint;
}

struct UniformTimingPolicy {
    latency_range : (uint, uint),
}
impl TimingPolicy for UniformTimingPolicy {
    fn network_latency(&self, _from: ServerID, _to: ServerID) -> uint {
        match self.latency_range {
          (min, max) => randrange(min, max)
        }
    }
}

struct Partition {
    servers: std::hashmap::HashSet<ServerID>,
    timing: ~TimingPolicy,
}

impl Partition {
    fn new(ids: &[ServerID], timing: ~TimingPolicy) -> Partition {
        Partition {
            servers: newHashSet(ids),
            timing: timing,
        }
    }
    fn newInt(ids: &[uint], timing: ~TimingPolicy) -> Partition {
        Partition {
            servers: newHashSet(ids.map(|id| ServerID(*id))),
            timing: timing,
        }
    }
}

fn newHashSet<T: Clone + IterBytes + Hash + Eq>(elements: &[T]) -> std::hashmap::HashSet<T> {
    let mut set = std::hashmap::HashSet::new();
    for e in elements.iter() {
        set.insert(e.clone());
    }
    return set;
}

struct PartitionedTimingPolicy(~[Partition]);

impl TimingPolicy for PartitionedTimingPolicy {
    fn network_latency(&self, from: ServerID, to: ServerID) -> uint {
        for partition in self.iter() {
            if partition.servers.contains(&from) &&
               partition.servers.contains(&to) {
                return partition.timing.network_latency(from, to);
            }
        }
        return NEVER;
    }
}

impl Environment {
    fn new(timing: ~TimingPolicy) -> Environment {
        Environment {
            clock: Time(0),
            network: ~[],
            timing: timing,
        }
    }
    fn make_time(&self, min: uint, max: uint) -> Time {
        self.clock + randrange(min, max)
    }
    fn multicast(&mut self, from: ServerID, to: &[ServerID], body: &MessageBody) {
        for peer in to.iter() {
            let m = ~Message {
                from: from,
                to: *peer,
                deliver: self.clock + self.timing.network_latency(from, *peer),
                body: *body,
            };
            self.network.push(m);
        }
    }
    fn reply(&mut self, request: &Message, response_body: &MessageBody) {
        let m = ~Message {
            from: request.to,
            to: request.from,
            deliver: self.clock + self.timing.network_latency(request.to, request.from),
            body: *response_body,
        };
        self.network.push(m);
    }
    fn pop_ready_messages(&mut self) -> ~[~Message] {
        if !self.network.iter().any(|msg| msg.deliver <= self.clock) {
            return ~[];
        }
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
    AppendEntriesRequest {
        term: Term,
        seqno: uint,
    },
    AppendEntriesResponse {
        term: Term,
        seqno: uint,
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
                write!(f.buf, "RequestVoteRequest(term: {}, lastLogTerm: {}, lastLogIndex: {})",
                       term, lastLogTerm, lastLogIndex),
            RequestVoteResponse{term, granted} =>
                write!(f.buf, "RequestVoteResponse(term: {}, granted: {})",
                       term, granted),
            AppendEntriesRequest{term, seqno} =>
                write!(f.buf, "AppendEntriesRequest(term: {}, seqno: {})",
                       term, seqno),
            AppendEntriesResponse{term, seqno} =>
                write!(f.buf, "AppendEntriesResponse(term: {}, seqno: {})",
                       term, seqno),
        }
    }
}



enum ServerState {
    Follower  { timer: Time },
    Candidate { timer: Time, votes: uint },
    Leader    { timer: Time, heartbeat_seqno: uint, acks: uint, start_time: Time},
}


impl fmt::Default for ServerState {
    fn fmt(state: &ServerState, f: &mut fmt::Formatter) {
        match *state {
            Follower{timer} => {
                write!(f.buf, "Follower(timer: {})",
                       timer)
            },
            Candidate{timer, votes} => {
                write!(f.buf, "Candidate(timer: {}, votes: {})",
                       timer, votes)
            },
            Leader{timer, heartbeat_seqno, acks, start_time} => {
                write!(f.buf, "Leader(timer: {}, seqno: {}, acks: {}, start_time: {})",
                       timer, heartbeat_seqno, acks, start_time)
            },
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
            state: Follower { timer: env.make_time(150, 299) },
            log: Log::new(),
            vote: None,
        }
    }
    fn tick(&mut self, env: &mut Environment) {
        let mut need_step_down = false;
        match self.state {
            Follower  {timer, _} |
            Candidate {timer, _} => {
                if timer <= env.clock {
                    self.term = Term(*self.term + 1);
                    self.vote = None;
                    self.state = Candidate {
                        timer: env.make_time(150, 299),
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
            Leader {timer: ref mut timer, heartbeat_seqno: ref mut heartbeat_seqno, acks: ref mut acks, _} => {
                if *timer <= env.clock {
                    if (*acks > (self.peers.len() + 1) / 2) {
                        *heartbeat_seqno += 1;
                        *acks = 0;
                        *timer = env.make_time(75, 75);
                        env.multicast(self.id, self.peers, &AppendEntriesRequest {
                            term: self.term,
                            seqno: *heartbeat_seqno,
                        });
                    } else {
                        need_step_down = true;
                    }
                }
            },
        }
        if need_step_down {
            println!("Failed to maintain leadership: {}", *self);
            self.step_down(env, self.term)
        }
    }
    fn step_down(&mut self, env: &Environment, term: Term) {
        self.term = term;
        self.vote = None;
        self.state = Follower {
            timer: env.make_time(150, 299),
        };
    }

    fn try_become_leader(&mut self, env: &mut Environment) {
        match self.state {
            Follower {_} => {},
            Candidate {votes, _} => {
                if votes > (self.peers.len() + 1) / 2 {
                    self.state = Leader {
                        timer: env.make_time(75, 75),
                        heartbeat_seqno: 0,
                        acks: 1,
                        start_time: env.clock,
                    };
                    env.multicast(self.id, self.peers, &AppendEntriesRequest {
                        term: self.term,
                        seqno: 0,
                    });
                }
            },
            Leader {_} => {},
        }
    }


    fn stable_leader_start_time(&self) -> Option<Time> {
        match self.state {
            Leader {heartbeat_seqno, start_time, _} if heartbeat_seqno > 4 => Some(start_time),
            _ => None,
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
                        Leader {_} => {},
                    }
                    self.try_become_leader(env)
                } else if term > self.term {
                    self.step_down(env, term);
                }
            },
            AppendEntriesRequest {term, seqno} => {
                if term == self.term {
                    self.state = Follower {
                        timer: env.make_time(150, 299),
                    };
                } else if term > self.term {
                    self.step_down(env, term);
                }
                env.reply(msg, &AppendEntriesResponse {
                    term: self.term,
                    seqno: seqno,
                });
            },
            AppendEntriesResponse {term, seqno} => {
                if term == self.term {
                    match self.state {
                        Follower {_} | Candidate {_} => {},
                        Leader { heartbeat_seqno, acks: ref mut acks, _ } => {
                            if seqno == heartbeat_seqno {
                                *acks += 1;
                            }
                        },
                    }
                }
            },
        }
    }

} // Server impl

impl fmt::Default for Server {
    fn fmt(server: &Server, f: &mut fmt::Formatter) {
        write!(f.buf, "Server {}: {}", *server.id, server.state)
    }
}

struct Cluster(~[Server]);

impl Cluster {
    fn new(env: &Environment, num_servers: uint) -> Cluster {
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
        return Cluster(servers);
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
        getopts::optopt("samples"),
        getopts::optopt("servers"),
        getopts::optopt("tasks"),
        getopts::optopt("timing"),
    ];
    let matches = match getopts::getopts(args.tail(), opts) {
        Ok(m) => { m },
        Err(f) => { fail!(f.to_err_msg()) },
    };
    let usage = || {
        println!("Usage: {} [options]", args[0]);
        println("-h, --help       Print this help message");
        println("--samples=N      Number of simulations (default 10,000)");
        println("--servers=N      Simulate cluster with N servers");
        println("--tasks=N        Number of parallel jobs (default 1)");
        println("--timing=POLICY  Number of timing policy (default LAN)");
    };
    if matches.opt_present("h") || matches.opt_present("help") {
        usage();
        return;
    }
    if !matches.free.is_empty() {
        usage();
        fail!(format!("Extra arguments: {}", matches.free.len()));
    }
    let num_samples : uint = match matches.opt_str("samples") {
        Some(s) => { match std::from_str::from_str(s) {
            Some(i) => { i },
            None => {
                usage();
                fail!(format!("Couldn't parse number of samples from '{}'", s));
            },
        }},
        None => { 10000 },
    };
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
    let num_tasks : uint = match matches.opt_str("tasks") {
        Some(s) => { match std::from_str::from_str(s) {
            Some(i) => { i },
            None => {
                usage();
                fail!(format!("Couldn't parse number of tasks from '{}'", s));
            },
        }},
        None => { 1 },
    };
    let timing : ~str = match matches.opt_str("timing") {
        Some(s) => { s },
        None => { ~"LAN" },
    };

    println!("Servers: {}", num_servers)
    println!("Tasks:   {}", num_tasks)
    println!("Trials:  {}", num_samples)
    println!("Timing:  {}", timing)

    let start_ns = extra::time::precise_time_ns();

    let (port, chan): (Port<~[Time]>, Chan<~[Time]>) = stream();
    let chan = std::comm::SharedChan::new(chan);
    for tid in range(0, num_tasks) {
        let child_chan = chan.clone();
        let child_timing = timing.clone();
        do spawn || {
            let mut samples = ~[];
            let n = if tid == 0 {
                // get the odd one left over
                num_samples - (num_samples / num_tasks) * (num_tasks - 1)
            } else {
                num_samples / num_tasks
            };
            samples.reserve(n);
            do n.times {
                let p = make_timing_policy(child_timing);
                let sample = simulate(num_servers, p);
                samples.push(sample);
            }
            extra::sort::tim_sort(samples);
            child_chan.send(samples);
        }
    }

    let mut samples = ~[];
    samples.reserve(num_samples);
    do num_tasks.times {
        samples.push_all(port.recv());
    }
    extra::sort::tim_sort(samples);

    let end_ns = extra::time::precise_time_ns();
    let elapsed_ns = end_ns - start_ns;

    println!("Min:     {} ticks", samples[0]);
    println!("Median:  {} ticks", samples[samples.len()/2]);
    println!("Max:     {} ticks", samples[samples.len()-1]);
    println!("Wall:    {:.2f} s",     elapsed_ns as f64 / 1e9);
}

fn make_timing_policy(name: &str) -> ~TimingPolicy {
    return match name {
        "Down" => ~UniformTimingPolicy {
                    latency_range: (NEVER, NEVER),
        } as ~TimingPolicy,
        "LAN" => ~UniformTimingPolicy {
                    latency_range: (2, 5),
        } as ~TimingPolicy,
        "P1" => ~PartitionedTimingPolicy(~[
                        Partition::newInt([1, 2], make_timing_policy("Down")),
                        Partition::newInt([3, 4, 5], make_timing_policy("LAN")),
        ]) as ~TimingPolicy,
        _ => fail!("Unknown timing policy name: {}", name),
    }
}


fn simulate(num_servers: uint, timing_policy: ~TimingPolicy) -> Time {
    let env = &mut Environment::new(timing_policy);
    let mut cluster = Cluster::new(env, num_servers);
    let mut end = None;
    while end.is_none() {
        env.clock = Time(*env.clock + 1);
        for server in cluster.mut_iter() {
            server.tick(env);
        }
        for msg in env.pop_ready_messages().move_iter() {
            cluster.deliver(env, msg);
        }
        for server in cluster.iter() {
            end = server.stable_leader_start_time();
            if end.is_some() {
                break;
            }
        }
        /*
        // useful for debugging:
        println!("Tick: {}", env.clock);
        for server in cluster.iter() {
            println!("{}", *server);
        }
        for message in env.network.iter() {
            println!("{}", **message);
        }
        println!("");
        */
    }
    return end.unwrap();
}

