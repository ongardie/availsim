#[feature(globs)];
extern mod std;
use basics::*;
use policies::TimingPolicy;
use raft::{Message, MessageBody, Configuration, Server};
use std::hashmap::HashSet;
use std::fmt;

struct SimMessage {
    deliver: Time,
    msg: Message,
}

impl fmt::Default for SimMessage {
    fn fmt(msg: &SimMessage, f: &mut fmt::Formatter) {
        write!(f.buf, "{} delayed until {}", msg.msg, msg.deliver);
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
    pub fn make_time(&self, min: uint, max: uint) -> Time {
        self.clock + randrange(min, max)
    }
    pub fn multicast(&mut self, from: ServerID, to: &Configuration, body: &MessageBody) {
        for l in to.iter() {
            for peer in l.iter() {
                if *peer != from {
                    let m = SimMessage {
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
    }
    pub fn reply(&mut self, request: &Message, response_body: &MessageBody) {
        let m = SimMessage {
            deliver: self.clock + self.timing.network_latency(request.to, request.from),
            msg: Message {
                from: request.to,
                to: request.from,
                body: *response_body,
            },
        };
        self.network.push(m);
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
                                      ServerID(6), ServerID(7)]);
                let new = newHashSet([ServerID(1), ServerID(2), ServerID(3),
                                      ServerID(4), ServerID(5)]);
                let mut servers = ~[];
                servers.push(Server::new(ServerID(1), Configuration(~[new.clone()]), env, algorithm));
                servers.push(Server::new(ServerID(2), Configuration(~[new.clone()]), env, algorithm));
                servers.push(Server::new(ServerID(3), Configuration(~[new.clone()]), env, algorithm));
                servers.push(Server::new(ServerID(4), Configuration(~[new.clone()]), env, algorithm));
                servers.push(Server::new(ServerID(5), Configuration(~[new.clone()]), env, algorithm));
                servers.push(Server::new(ServerID(6), Configuration(~[old.clone()]), env, algorithm));
                servers.push(Server::new(ServerID(7), Configuration(~[old.clone()]), env, algorithm));
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
                        ServerID(6) | ServerID(7) => Index(0),
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
                        ServerID(4) | ServerID(5) | ServerID(6) | ServerID(7) => Index(0),
                        _ => Index(*length),
                    };
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

pub fn simulate(cluster_policy: &str,
                timing_policy: ~TimingPolicy,
                log_lengths: &str,
                algorithm: &str,
                terms: &str,
                max_ticks: Time) -> Time {
    let env = &mut Environment::new(timing_policy);
    let mut cluster = Cluster::new(env, cluster_policy, algorithm);
    cluster.set_log_lengths(log_lengths);
    cluster.set_terms(terms);

    let mut end = None;
    let mut ready_msgs = ~[]; // declared out here to avoid mallocs
    while end.is_none() {
        env.clock = Time(*env.clock + 1);
        if env.clock >= max_ticks {
            end = Some(max_ticks);
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
            end = server.stable_leader_start_time();
            if end.is_some() {
                break;
            }
        }

        // useful for debugging:
        if false {
            println!("Tick: {}", env.clock);
            for server in cluster.iter() {
                println!("{}", *server);
            }
            // Stack overflow?
            //for message in env.network.iter() {
            //    println!("{}", *message);
            //}
            println!("");
        }

    }
    return end.unwrap();
}
