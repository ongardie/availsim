extern mod std;

use std::fmt;
use std::hashmap::HashSet;

pub fn randrange(min : uint, max : uint) -> uint {
    use std::rand::distributions::{IndependentSample, Range};
    if min == max {
        min
    } else {
        let range = Range::new(min, max);
        range.ind_sample(&mut std::rand::task_rng())
    }
}

#[deriving(Eq, TotalEq, IterBytes, Clone, TotalOrd, ToStr)]
pub struct ServerID(uint);

impl fmt::Default for ServerID {
    fn fmt(id: &ServerID, f: &mut fmt::Formatter) {
        let ServerID(v) = *id;
        write!(f.buf, "{}", v)
    }
}


#[deriving(Eq, Ord, Clone)]
pub struct Term(uint);

impl Term {
    pub fn next(&self) -> Term {
        let Term(v) = *self;
        Term(v + 1)
    }
}

impl fmt::Default for Term {
    fn fmt(term: &Term, f: &mut fmt::Formatter) {
        let Term(v) = *term;
        write!(f.buf, "{}", v)
    }
}

#[deriving(Ord, Clone)]
pub struct Index(uint);

impl fmt::Default for Index {
    fn fmt(index: &Index, f: &mut fmt::Formatter) {
        let Index(v) = *index;
        write!(f.buf, "{}", v)
    }
}

#[deriving(Eq, TotalEq, Ord, TotalOrd, Clone)]
pub struct Time(uint);

// An integer big enough to represent infinity in the simulations, but small
// enough that it'll never overflow.
pub static NEVER : Time = Time(1<<30);
pub static INFINITY : uint = 1<<30;


impl Add<uint, Time> for Time {
    fn add(&self, rhs: &uint) -> Time {
        let Time(v) = *self;
        Time(v + *rhs)
    }
}

impl fmt::Default for Time {
    fn fmt(time: &Time, f: &mut fmt::Formatter) {
        let Time(v) = *time;
        write!(f.buf, "{}", v)
    }
}

pub fn newHashSet<T: Clone + IterBytes + Hash + Eq>(elements: &[T]) -> HashSet<T> {
    let mut set = HashSet::new();
    for e in elements.iter() {
        set.insert(e.clone());
    }
    return set;
}

