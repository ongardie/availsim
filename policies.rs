#[feature(globs)];
use basics::*;
use std::hashmap::HashSet;

mod basics;

pub trait TimingPolicy {
    fn network_latency(&self, from: ServerID, to: ServerID) -> uint;
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

struct PartitionedTimingPolicy(~[Partition]);

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


pub fn make_timing_policy(name: &str) -> ~TimingPolicy {
    return match name {
        "Down" => ~UniformTimingPolicy {
                    latency_range: (NEVER, NEVER),
        } as ~TimingPolicy,
        "LAN" => ~UniformTimingPolicy {
                    latency_range: (2, 5),
        } as ~TimingPolicy,
        "WAN" => ~UniformTimingPolicy {
                    latency_range: (30, 70),
        } as ~TimingPolicy,
        "P1" => ~PartitionedTimingPolicy(~[
                    Partition::newInt([1],          make_timing_policy("Down")),
                    Partition::newInt([2, 3, 4, 5], make_timing_policy("LAN")),
        ]) as ~TimingPolicy,
        "P2" => ~PartitionedTimingPolicy(~[
                    Partition::newInt([1, 2],       make_timing_policy("Down")),
                    Partition::newInt([3, 4, 5],    make_timing_policy("LAN")),
        ]) as ~TimingPolicy,
        "L1" => ~BadLinks {
            links: ~[Link{ from: ServerID(1), to: ServerID(2),
                           timing: make_timing_policy("WAN") }],
            other: make_timing_policy("LAN"),
        } as ~TimingPolicy,
        "Deian" => ~PartitionedTimingPolicy(~[
                    Partition::newInt([1,2,3,4],   make_timing_policy("LAN")),
                    Partition::newInt([1,2,3,4,5], make_timing_policy("WAN")),
        ]) as ~TimingPolicy,
        _ => fail!("Unknown timing policy name: {}", name),
    }
}


