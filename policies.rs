extern mod std;

use basics::*;
use std::hashmap::HashSet;

mod basics;

pub trait TimingPolicy {
    fn network_latency(&self, from: ServerID, to: ServerID) -> uint;
}

struct Uniform(uint, uint);
impl TimingPolicy for Uniform {
    fn network_latency(&self, _from: ServerID, _to: ServerID) -> uint {
        match *self {
          Uniform(min, max) => randrange(min, max)
        }
    }
}

struct Stochastic {
    afrac: f64,
    a: ~TimingPolicy,
    b: ~TimingPolicy,
}
impl TimingPolicy for Stochastic {
    fn network_latency(&self, from: ServerID, to: ServerID) -> uint {
        use std::rand::distributions::{IndependentSample, Range};
        let range = Range::new(0.0, 1.0);
        let sample = range.ind_sample(&mut std::rand::task_rng());
        if sample < self.afrac {
            self.a.network_latency(from, to)
        } else {
            self.b.network_latency(from, to)
        }
    }
}

struct Partitions(~[Partition]);
impl Partitions {
    fn get<'a>(&'a self) -> &'a ~[Partition] {
        let Partitions(ref v) = *self;
        v
    }
}

struct Partition {
    servers: HashSet<ServerID>,
    timing: ~TimingPolicy,
}

impl Partition {
    fn new(ids: &[ServerID], timing: ~TimingPolicy) -> Partition {
        Partition {
            servers: newHashSet(ids),
            timing: timing,
        }
    }
}

impl TimingPolicy for Partitions {
    fn network_latency(&self, from: ServerID, to: ServerID) -> uint {
        for partition in self.get().iter() {
            if partition.servers.contains(&from) &&
               partition.servers.contains(&to) {
                return partition.timing.network_latency(from, to);
            }
        }
        return INFINITY;
    }
}



struct Link {
    from: ServerID,
    to: ServerID,
    timing: ~TimingPolicy,
}
macro_rules! link(
    ($name:expr: $from:expr -> $to:expr) => (
        Link {
            from: ServerID($from),
            to: ServerID($to),
            timing: make(stringify!($name))
        }
    );
)

struct Links {
    links: ~[Link],
    other: ~TimingPolicy,
}

impl TimingPolicy for Links {
    fn network_latency(&self, from: ServerID, to: ServerID) -> uint {
        for link in self.links.iter() {
            if link.from == from && link.to == to {
                return link.timing.network_latency(from, to);
            }
        }
        return self.other.network_latency(from, to);
    }
}

macro_rules! partition(
    ($name:expr: $($id:expr) +) => (
        Partition::new([$(ServerID($id),)+], make(stringify!($name)))
    );
)

pub fn make(name: &str) -> ~TimingPolicy {
    return match name {
        "Delay1pct" => ~Stochastic {
            afrac: 0.99,
            a: ~Uniform(2000, 5000) as ~TimingPolicy,
            b: ~Uniform(5000, 100000) as ~TimingPolicy,
        } as ~TimingPolicy,
        "Down" => ~Uniform(INFINITY, INFINITY)
        as ~TimingPolicy,
        "LAN" => ~Uniform(2000, 5000)
        as ~TimingPolicy,
        "RAMCloud" => ~Uniform(5, 10)
        as ~TimingPolicy,
        "WAN" => ~Uniform(10000, 20000)
        as ~TimingPolicy,
        "fWAN" => ~Uniform(15000, 15000)
        as ~TimingPolicy,
        "P1" => ~Partitions(~[
                    partition!(Down: 1),
                    partition!(LAN:  2 3 4 5),
        ]) as ~TimingPolicy,
        "P1-RAMCloud" => ~Partitions(~[
                    partition!(Down: 1),
                    partition!(RAMCloud:  2 3 4 5),
        ]) as ~TimingPolicy,
        "P1-WAN" => ~Partitions(~[
                    partition!(Down: 1),
                    partition!(WAN:  2 3 4 5),
        ]) as ~TimingPolicy,
        "P1-fWAN" => ~Partitions(~[
                    partition!(Down: 1),
                    partition!(fWAN:  2 3 4 5),
        ]) as ~TimingPolicy,
        "P2" => ~Partitions(~[
                    partition!(Down: 1 2),
                    partition!(LAN:  3 4 5),
        ]) as ~TimingPolicy,
        "P2-WAN" => ~Partitions(~[
                    partition!(Down: 1 2),
                    partition!(WAN:  3 4 5),
        ]) as ~TimingPolicy,
        "P2-fWAN" => ~Partitions(~[
                    partition!(Down: 1 2),
                    partition!(fWAN:  3 4 5),
        ]) as ~TimingPolicy,
        "P2-RAMCloud" => ~Partitions(~[
                    partition!(Down: 1 2),
                    partition!(RAMCloud:  3 4 5),
        ]) as ~TimingPolicy,
        "L1" => ~Links {
            links: ~[link!(WAN: 1 -> 2)],
            other: make("LAN"),
        } as ~TimingPolicy,
        "Deian" => ~Partitions(~[
                    partition!(LAN: 1 2 3 4),
                    partition!(WAN: 1 2 3 4 5),
        ]) as ~TimingPolicy,
        "BadRecv" => ~Links {
            links: ~[
                link!(Down: 2 -> 1),
                link!(Down: 3 -> 1),
                link!(Down: 4 -> 1),
                link!(Down: 5 -> 1),
                link!(Down: 6 -> 1),
                link!(Down: 7 -> 1),
                link!(Down: 8 -> 1),
                link!(Down: 9 -> 1),
            ],
            other: make("LAN"),
        } as ~TimingPolicy,
        "BadRecv-RAMCloud" => ~Links {
            links: ~[
                link!(Down: 2 -> 1),
                link!(Down: 3 -> 1),
                link!(Down: 4 -> 1),
                link!(Down: 5 -> 1),
                link!(Down: 6 -> 1),
                link!(Down: 7 -> 1),
                link!(Down: 8 -> 1),
                link!(Down: 9 -> 1),
            ],
            other: make("RAMCloud"),
        } as ~TimingPolicy,
        _ => fail!("Unknown timing policy name: {}", name),
    }
}


