use std::fmt;
use std::rand;
use std::hashmap::HashSet;

pub fn randrange(min : uint, max : uint) -> uint {
    min + rand::random::<uint>() % (max - min + 1)
}

#[deriving(Eq, IterBytes, Clone, Ord)]
pub struct ServerID(uint);

impl fmt::Default for ServerID {
    fn fmt(id: &ServerID, f: &mut fmt::Formatter) {
        write!(f.buf, "{}", **id)
    }
}


#[deriving(Eq, Ord)]
pub struct Term(uint);

impl fmt::Default for Term {
    fn fmt(term: &Term, f: &mut fmt::Formatter) {
        write!(f.buf, "{}", **term)
    }
}


#[deriving(Ord)]
pub struct Index(uint);

impl fmt::Default for Index {
    fn fmt(index: &Index, f: &mut fmt::Formatter) {
        write!(f.buf, "{}", **index)
    }
}


#[deriving(Ord, Clone)]
pub struct Time(uint);

// An integer big enough to represent infinity in the simulations, but small
// enough that it'll never overflow.
pub static NEVER : uint = 1<<30;


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

pub fn newHashSet<T: Clone + IterBytes + Hash + Eq>(elements: &[T]) -> HashSet<T> {
    let mut set = HashSet::new();
    for e in elements.iter() {
        set.insert(e.clone());
    }
    return set;
}

