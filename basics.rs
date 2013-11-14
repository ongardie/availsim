use std::fmt;
use std::rand;

pub fn randrange(min : uint, max : uint) -> uint {
    min + rand::random::<uint>() % (max - min + 1)
}

#[deriving(Eq, IterBytes, Clone)]
pub struct ServerID(uint);

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
