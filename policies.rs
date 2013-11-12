#[feature(globs)];
use basics::*;
use std::hashmap::HashSet;

mod basics;

pub trait TimingPolicy {
    fn network_latency(&self, from: ServerID, to: ServerID) -> uint;
}

struct Uniform {
    latency_range : (uint, uint),
}
impl TimingPolicy for Uniform {
    fn network_latency(&self, _from: ServerID, _to: ServerID) -> uint {
        match self.latency_range {
          (min, max) => randrange(min, max)
        }
    }
}

struct Partitioned(~[Partition]);

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
    fn newInt(ids: &[uint], timing: ~TimingPolicy) -> Partition {
        Partition {
            servers: newHashSet(ids.map(|id| ServerID(*id))),
            timing: timing,
        }
    }
}

impl TimingPolicy for Partitioned {
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

struct BadLinks {
    links: ~[Link],
    other: ~TimingPolicy,
}

impl TimingPolicy for BadLinks {
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


pub fn make(name: &str) -> ~TimingPolicy {
    return match name {
        "Down" => ~Uniform {
                    latency_range: (NEVER, NEVER),
        } as ~TimingPolicy,
        "LAN" => ~Uniform {
                    latency_range: (2, 5),
        } as ~TimingPolicy,
        "WAN" => ~Uniform {
                    latency_range: (30, 70),
        } as ~TimingPolicy,
        "P1" => ~Partitioned(~[
                    Partition::newInt([1],          make("Down")),
                    Partition::newInt([2, 3, 4, 5], make("LAN")),
        ]) as ~TimingPolicy,
        "P2" => ~Partitioned(~[
                    Partition::newInt([1, 2],       make("Down")),
                    Partition::newInt([3, 4, 5],    make("LAN")),
        ]) as ~TimingPolicy,
        "L1" => ~BadLinks {
            links: ~[Link{ from: ServerID(1), to: ServerID(2),
                           timing: make("WAN") }],
            other: make("LAN"),
        } as ~TimingPolicy,
        "Deian" => ~Partitioned(~[
                    Partition::newInt([1,2,3,4],   make("LAN")),
                    Partition::newInt([1,2,3,4,5], make("WAN")),
        ]) as ~TimingPolicy,
        _ => fail!("Unknown timing policy name: {}", name),
    }
}


