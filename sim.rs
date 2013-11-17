#[feature(globs)];
extern mod std;
use basics::*;
use policies::TimingPolicy;
use raft::{Message, MessageBody, Server};

struct SimMessage {
    deliver: Time,
    msg: Message,
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
    pub fn multicast(&mut self, from: ServerID, to: &[ServerID], body: &MessageBody) {
        for peer in to.iter() {
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
    fn new(env: &Environment, num_servers: uint, algorithm: &str) -> Cluster {
        let mut servers = ~[];
        for i in range(0, num_servers) {
            let mut peers = ~[];
            for j in range(0, num_servers) {
                if i != j {
                    peers.push(ServerID(j + 1));
                }
            }
            let s = Server::new(ServerID(i + 1), peers, env, algorithm);
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
            _ => fail!("Unknown log length policy: {}", policy)
        }
    }

    fn deliver(&mut self, env: &mut Environment, msg: &Message) {
        self[*msg.to - 1].handle(env, msg);
    }
}

pub fn simulate(num_servers: uint,
                timing_policy: ~TimingPolicy,
                log_lengths: &str,
                algorithm: &str) -> Time {
    let env = &mut Environment::new(timing_policy);
    let mut cluster = Cluster::new(env, num_servers, algorithm);
    cluster.set_log_lengths(log_lengths);
    let mut end = None;
    let mut ready_msgs = ~[]; // declared out here to avoid mallocs
    while end.is_none() {
        env.clock = Time(*env.clock + 1);
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
