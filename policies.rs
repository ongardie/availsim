#[feature(globs)];
#[feature(macro_rules)];

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
        let sample = range.ind_sample(std::rand::task_rng());
        if sample < self.afrac {
            self.a.network_latency(from, to)
        } else {
            self.b.network_latency(from, to)
        }
    }
}

struct Partitions(~[Partition]);

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
        for partition in self.iter() {
            if partition.servers.contains(&from) &&
               partition.servers.contains(&to) {
                return partition.timing.network_latency(from, to);
            }
        }
        return NEVER;
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

fn newHashSet<T: Clone + IterBytes + Hash + Eq>(elements: &[T]) -> HashSet<T> {
    let mut set = HashSet::new();
    for e in elements.iter() {
        set.insert(e.clone());
    }
    return set;
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
            a: ~Uniform(2, 5) as ~TimingPolicy,
            b: ~Uniform(5, 100) as ~TimingPolicy,
        } as ~TimingPolicy,
        "Down" => ~Uniform(NEVER, NEVER)
        as ~TimingPolicy,
        "LAN" => ~Uniform(2, 5)
        as ~TimingPolicy,
        "WAN" => ~Uniform(30, 70)
        as ~TimingPolicy,
        "P1" => ~Partitions(~[
                    partition!(Down: 1),
                    partition!(LAN:  2 3 4 5),
        ]) as ~TimingPolicy,
        "P2" => ~Partitions(~[
                    partition!(Down: 1 2),
                    partition!(LAN:  3 4 5),
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
            ],
            other: make("LAN"),
        } as ~TimingPolicy,
        _ => fail!("Unknown timing policy name: {}", name),
    }
}

