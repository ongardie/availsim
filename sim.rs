#[feature(globs)];
extern mod std;
use std::fmt;
use basics::*;
use policies::TimingPolicy;
use raft::{Message, MessageBody, Server};

struct SimMessage {
    deliver: Time,
    msg: ~Message,
}

struct Environment {
    clock: Time,
    network: ~[~SimMessage],
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
            let m = ~SimMessage {
                deliver: self.clock + self.timing.network_latency(from, *peer),
                msg: ~Message {
                    from: from,
                    to: *peer,
                    body: *body,
                },
            };
            self.network.push(m);
        }
    }
    pub fn reply(&mut self, request: &Message, response_body: &MessageBody) {
        let m = ~SimMessage {
            deliver: self.clock + self.timing.network_latency(request.to, request.from),
            msg: ~Message {
                from: request.to,
                to: request.from,
                body: *response_body,
            },
        };
        self.network.push(m);
    }
    pub fn pop_ready_messages(&mut self) -> ~[~Message] {
    // TODO: wow this needs a rewrite
        if !self.network.iter().any(|msg| msg.deliver <= self.clock) {
            return ~[];
        }
        let mut all = ~[];
        std::util::swap(&mut all, &mut self.network);
        let (ready, notready) = all.partition(|msg| msg.deliver <= self.clock);
        self.network = notready;
        let mut ready2 = ~[];
        for m in ready.move_iter() {
            ready2.push(m.msg);
        }
        return ready2;
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

pub fn simulate(num_servers: uint, timing_policy: ~TimingPolicy) -> Time {
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
