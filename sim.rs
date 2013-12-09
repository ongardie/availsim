extern mod std;
extern mod extra;

use basics::*;
use policies::TimingPolicy;
use raft::{Message, MessageBody, Configuration, Server};
use std::hashmap::HashSet;
use std::fmt;

#[deriving(Clone)]
struct SimMessage {
    sent: Time,
    deliver: Time,
    msg: Message,
}

impl fmt::Default for SimMessage {
    fn fmt(msg: &SimMessage, f: &mut fmt::Formatter) {
        write!(f.buf, "Message(sent: {}, deliver: {}, {})", msg.sent, msg.deliver, msg.msg);
    }
}


struct Environment {
    clock: Time,
    network: ~[SimMessage],
    timing: ~TimingPolicy,
}

impl Environment {
    fn new(timing: ~TimingPolicy) -> Environment {
        Environment {
            clock: Time(0),
            network: ~[],
            timing: timing,
        }
    }
    pub fn make_time(&self, range: (uint, uint)) -> Time {
        self.clock + randrange(range.first(), range.second())
    }
    fn sort_network(&mut self) {
        extra::sort::quick_sort(self.network, |x,y| x.deliver <= y.deliver);
    }
    pub fn multicast(&mut self, from: ServerID, to: &Configuration, body: &MessageBody) {
        for l in to.iter() {
            for peer in l.iter() {
                if *peer != from {
                    let m = SimMessage {
                        sent: self.clock,
                        deliver: self.clock + self.timing.network_latency(from, *peer),
                        msg: Message {
                            from: from,
                            to: *peer,
                            body: *body,
                        },
                    };
                    self.network.push(m);
                }
            }
        }
        self.sort_network();
    }
    pub fn reply(&mut self, request: &Message, response_body: &MessageBody) {
        let m = SimMessage {
            sent: self.clock,
            deliver: self.clock + self.timing.network_latency(request.to, request.from),
            msg: Message {
                from: request.to,
                to: request.from,
                body: *response_body,
            },
        };
        self.network.push(m);
        self.sort_network();
    }
    pub fn get_ready_messages(&mut self, out: &mut ~[Message]) {
        let mut i = 0;
        while i < self.network.len() {
            if self.network[i].deliver <= self.clock {
                out.push(self.network.swap_remove(i).msg);
            } else {
                i += 1;
            }
        }
        self.sort_network();
    }

    fn next_tick(&self) -> Time {
        let mut r = NEVER;
        for m in self.network.iter() {
            r = std::cmp::min(r, m.deliver)
        }
        return r;
    }
}

struct Cluster(~[Server]);

impl Cluster {
    fn new(env: &Environment, policy: &str, algorithm: &str) -> Cluster {
        match policy {
            "2"  => Cluster::flat(env,  2, algorithm),
            "3"  => Cluster::flat(env,  3, algorithm),
            "4"  => Cluster::flat(env,  4, algorithm),
            "5"  => Cluster::flat(env,  5, algorithm),
            "6"  => Cluster::flat(env,  6, algorithm),
            "7"  => Cluster::flat(env,  7, algorithm),
            "8"  => Cluster::flat(env,  8, algorithm),
            "9"  => Cluster::flat(env,  9, algorithm),
            "10" => Cluster::flat(env, 10, algorithm),
            "11" => Cluster::flat(env, 11, algorithm),
            "12" => Cluster::flat(env, 12, algorithm),
            "13" => Cluster::flat(env, 13, algorithm),
            "14" => Cluster::flat(env, 14, algorithm),
            "15" => Cluster::flat(env, 15, algorithm),
            "5-2+2" => {
                let old = newHashSet([ServerID(1), ServerID(2), ServerID(3),
                                      ServerID(4), ServerID(5)]);
                let new = newHashSet([ServerID(3), ServerID(4), ServerID(5),
                                      ServerID(6), ServerID(7)]);
                let mut servers = ~[];
                servers.push(Server::new(ServerID(1), Configuration(~[old.clone()]), env, algorithm));
                servers.push(Server::new(ServerID(2), Configuration(~[old.clone()]), env, algorithm));
                servers.push(Server::new(ServerID(3), Configuration(~[new.clone()]), env, algorithm));
                servers.push(Server::new(ServerID(4), Configuration(~[new.clone()]), env, algorithm));
                servers.push(Server::new(ServerID(5), Configuration(~[new.clone()]), env, algorithm));
                servers.push(Server::new(ServerID(6), Configuration(~[new.clone()]), env, algorithm));
                servers.push(Server::new(ServerID(7), Configuration(~[new.clone()]), env, algorithm));
                Cluster(servers)
            },
            "1-5to3-7:1old2both3old4both5both6old7both" => {
                let old = newHashSet([ServerID(1), ServerID(2), ServerID(3),
                                      ServerID(4), ServerID(5)]);
                let new = newHashSet([ServerID(3), ServerID(4), ServerID(5),
                                      ServerID(6), ServerID(7)]);
                let mut servers = ~[];
                servers.push(Server::new(ServerID(1), Configuration(~[old.clone()]), env, algorithm));
                servers.push(Server::new(ServerID(2), Configuration(~[old.clone(), new.clone()]), env, algorithm));
                servers.push(Server::new(ServerID(3), Configuration(~[old.clone()]), env, algorithm));
                servers.push(Server::new(ServerID(4), Configuration(~[old.clone(), new.clone()]), env, algorithm));
                servers.push(Server::new(ServerID(5), Configuration(~[old.clone(), new.clone()]), env, algorithm));
                servers.push(Server::new(ServerID(6), Configuration(~[old.clone()]), env, algorithm));
                servers.push(Server::new(ServerID(7), Configuration(~[old.clone(), new.clone()]), env, algorithm));
                Cluster(servers)
            },
            "1-5to3-7:1old2both3old4new5new6old7new" => {
                let old = newHashSet([ServerID(1), ServerID(2), ServerID(3),
                                      ServerID(4), ServerID(5)]);
                let new = newHashSet([ServerID(3), ServerID(4), ServerID(5),
                                      ServerID(6), ServerID(7)]);
                let mut servers = ~[];
                servers.push(Server::new(ServerID(1), Configuration(~[old.clone()]), env, algorithm));
                servers.push(Server::new(ServerID(2), Configuration(~[old.clone(), new.clone()]), env, algorithm));
                servers.push(Server::new(ServerID(3), Configuration(~[old.clone()]), env, algorithm));
                servers.push(Server::new(ServerID(4), Configuration(~[new.clone()]), env, algorithm));
                servers.push(Server::new(ServerID(5), Configuration(~[new.clone()]), env, algorithm));
                servers.push(Server::new(ServerID(6), Configuration(~[old.clone()]), env, algorithm));
                servers.push(Server::new(ServerID(7), Configuration(~[new.clone()]), env, algorithm));
                Cluster(servers)
            },
            _ => fail!("Unknown cluster policy: {}", policy)
        }
    }

    fn flat(env: &Environment, num_servers: uint, algorithm: &str) -> Cluster {
        let mut servers = ~[];
        for i in range(0, num_servers) {
            let mut config = HashSet::new();
            for j in range(0, num_servers) {
                config.insert(ServerID(j + 1));
            }
            let s = Server::new(ServerID(i + 1), Configuration(~[config]), env, algorithm);
            servers.push(s);
        }
        return Cluster(servers);
    }

    fn set_log_lengths(&mut self, policy: &str) {
        match policy {
            "same" => for server in self.mut_iter() {
                server.lastLogIndex = Index(0);
            },
            "diff" => {
                use std::rand::Rng;
                let mut lengths = range(0, self.len()).to_owned_vec();
                std::rand::task_rng().shuffle_mut(lengths);
                for (server, length) in self.mut_iter().zip(lengths.iter()) {
                    server.lastLogIndex = Index(*length);
                }
            },
            "diff-oldstale" => {
                use std::rand::Rng;
                let mut lengths = range(1, self.len() + 1).to_owned_vec();
                std::rand::task_rng().shuffle_mut(lengths);
                for (server, length) in self.mut_iter().zip(lengths.iter()) {
                    server.lastLogIndex = match server.id {
                        ServerID(1) | ServerID(2) => Index(0),
                        _ => Index(*length),
                    };
                }
            },
            "diff-oldminstale" => {
                use std::rand::Rng;
                let mut lengths = range(1, self.len() + 1).to_owned_vec();
                std::rand::task_rng().shuffle_mut(lengths);
                for (server, length) in self.mut_iter().zip(lengths.iter()) {
                    server.lastLogIndex = match server.id {
                        ServerID(1) | ServerID(2) | ServerID(3) | ServerID(4) => Index(0),
                        _ => Index(*length),
                    };
                }
            },
            "diff-136stale" => {
                use std::rand::Rng;
                let mut lengths = range(1, self.len() + 1).to_owned_vec();
                std::rand::task_rng().shuffle_mut(lengths);
                for (server, length) in self.mut_iter().zip(lengths.iter()) {
                    server.lastLogIndex = match server.id {
                        ServerID(1) | ServerID(3) | ServerID(6) => Index(0),
                        _ => Index(*length),
                    };
                }
            },
            "1old2both3old4new5new6old7new" => {
              for server in self.mut_iter() {
                server.lastLogIndex = match server.id {
                  ServerID(1) | ServerID(3) | ServerID(6) => Index(1),
                  ServerID(2) => Index(2),
                  ServerID(4) | ServerID(5) | ServerID(7) => Index(3),
                  _ => fail!("Bad server"),
                }
              }
            },
            _ => fail!("Unknown log length policy: {}", policy)
        }
    }

    fn set_terms(&mut self, policy: &str) {
        match policy {
            "same" => for server in self.mut_iter() {
                server.term = Term(0);
            },
            "diff" => {
                use std::rand::Rng;
                let mut terms = range(0, self.len()).to_owned_vec();
                std::rand::task_rng().shuffle_mut(terms);
                for (server, term) in self.mut_iter().zip(terms.iter()) {
                    server.term = Term(*term);
                }
            },
            _ => fail!("Unknown term policy: {}", policy)
        }
    }

    fn deliver(&mut self, env: &mut Environment, msg: &Message) {
        self[*msg.to - 1].handle(env, msg);
    }
}

#[deriving(Clone)]
pub struct SimOpts {
    cluster: ~str,
    heartbeats: uint,
    timing: ~str,
    log_length: ~str,
    algorithm: ~str,
    terms: ~str,
    max_ticks: Time,
    trace: uint,
}

pub fn simulate(run: uint, opts: &SimOpts) -> Time {

    let mut tracefile = if run < opts.trace {
        let path = &std::path::posix::Path::new(format!("trace{:06u}.html", run));
        std::io::File::create(path)
    } else {
        None
    };
    let mut tracebwriter = match tracefile {
        Some(ref mut f) => {
            let writer = f as &mut Writer;
            Some(std::io::buffered::BufferedWriter::new(writer))
        },
        None => None,
    };
    let mut tracewriter = match tracebwriter {
        Some(ref mut w) => {
            Some(w as &mut Writer)
        },
        None => None,
    };

    let mut eventsfile = if run < opts.trace {
        let path = &std::path::posix::Path::new(format!("events{:06u}.csv", run));
        std::io::File::create(path)
    } else {
        None
    };
    let mut eventsbwriter = match eventsfile {
        Some(ref mut f) => {
            let writer = f as &mut Writer;
            Some(std::io::buffered::BufferedWriter::new(writer))
        },
        None => None,
    };
    let mut eventswriter = match eventsbwriter {
        Some(ref mut w) => {
            Some(w as &mut Writer)
        },
        None => None,
    };


    match eventswriter {
        Some(ref mut w) => {
            writeln!(*w, "time,server,state,term");
        },
        None => {},
    }

    let timing_policy = ::policies::make(opts.timing);
    let env = &mut Environment::new(timing_policy);
    let mut cluster = Cluster::new(env, opts.cluster, opts.algorithm);
    cluster.set_log_lengths(opts.log_length);
    cluster.set_terms(opts.terms);

    let mut end = None;
    let mut ready_msgs = ~[]; // declared out here to avoid mallocs
    let mut next_tick = Time(0);

    let mut last_tick_server_strs = ~[];
    let mut last_tick_server_states = ~[];
    for _ in range(0, cluster.len()) {
        last_tick_server_strs.push(~"");
        last_tick_server_states.push(('_', Term(0)));
    }

    while end.is_none() {

        env.clock = next_tick;
        next_tick = NEVER;

        if env.clock >= opts.max_ticks {
            end = Some(opts.max_ticks);
            break;
        }
        for server in cluster.mut_iter() {
            server.tick(env);
        }

        // deliver new messages
        env.get_ready_messages(&mut ready_msgs);
        for msg in ready_msgs.iter() {
            cluster.deliver(env, msg);
        }
        ready_msgs.truncate(0);

        for server in cluster.iter() {
            end = server.stable_leader_start_time(opts.heartbeats);
            if end.is_some() {
                break;
            }
        }

        match tracewriter {
            Some(ref mut w) => {
                writeln!(*w, "<div class=\"list-group-item\">");
                writeln!(*w, "<h4>{:0.03f} ms</h4>", (*env.clock as f64) / 1000.0);
                writeln!(*w, "<pre>");
                for (server, last) in cluster.iter().zip(last_tick_server_strs.mut_iter()) {
                    let s = format!("{}", *server);
                    if (*last != s) {
                        writeln!(*w, "<span class=\"new\">{}</span>", s);
                    } else {
                        writeln!(*w, "{}", s);
                    }
                    *last = s;
                }
                for message in env.network.iter() {
                    if message.sent == env.clock {
                        writeln!(*w, "<span class=\"new\">{}</span>", *message);
                    } else {
                        writeln!(*w, "{}", *message);
                    }
                }
                writeln!(*w, "</pre>");
                writeln!(*w, "</div>");
                writeln!(*w, "");
            },
            None => {},
        }

        match eventswriter {
            Some(ref mut w) => {
                for (server, last) in cluster.iter().zip(last_tick_server_states.mut_iter()) {
                    let s = (server.state.to_char(), server.term);
                    if (*last != s) {
                        writeln!(*w, "{},{},{},{}", env.clock, server.id, s.first(), s.second());
                    }
                    *last = s;
                }
            },
            None => {},
        }

        next_tick = std::cmp::min(next_tick, env.next_tick());
        for server in cluster.iter() {
            next_tick = std::cmp::min(next_tick, server.next_tick());
        }
        assert!(next_tick > env.clock,
                "next_tick ({}) <= env.clock ({})",
                next_tick,
                env.clock);

    }
    match eventswriter {
        Some(ref mut w) => w.flush(),
        None => {}
    }
    match tracewriter {
        Some(ref mut w) => w.flush(),
        None => {}
    }

    return end.unwrap();
}
