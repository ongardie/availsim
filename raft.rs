extern mod std;
extern mod extra;

use basics::*;
use std::fmt;
use sim::Environment;
use std::hashmap::HashSet;

static ELECTION_TIMEOUT : (uint, uint) = (100000, 199999);
static HEARTBEAT_TIMEOUT : (uint, uint) = (50000, 50000);

#[deriving(Clone)]
pub struct Message {
    from: ServerID,
    to: ServerID,
    body: MessageBody,
}

impl fmt::Default for Message {
    fn fmt(msg: &Message, f: &mut fmt::Formatter) {
        write!(f.buf, "{} to {}: {}",
               *msg.from,
               *msg.to,
               msg.body);
    }
}

#[deriving(Eq, Clone)]
enum VoteGranted {
    GRANTED,
    TERM_STALE,
    LOG_STALE,
    VOTED,
    LEASE,
}

impl fmt::Default for VoteGranted {
    fn fmt(g: &VoteGranted, f: &mut fmt::Formatter) {
        write!(f.buf, "{}", match *g {
            GRANTED    => "GRANTED",
            TERM_STALE => "TERM_STALE",
            LOG_STALE  => "LOG_STALE",
            VOTED      => "VOTED",
            LEASE      => "LEASE",
        });
    }
}


#[deriving(Clone)]
pub enum MessageBody {
    RequestVoteRequest {
        term: Term,
        lastLogIndex: Index,
    },
    RequestVoteResponse {
        term: Term,
        granted: VoteGranted,
        logOk: bool,
    },
    AppendEntriesRequest {
        term: Term,
        seqno: uint,
    },
    AppendEntriesResponse {
        term: Term,
        seqno: uint,
    },
}

impl fmt::Default for MessageBody {
    fn fmt(b: &MessageBody, f: &mut fmt::Formatter) {
        match *b {
            RequestVoteRequest{term, lastLogIndex} =>
                write!(f.buf, "RequestVoteRequest(term: {}, lastLogIndex: {})",
                       term, lastLogIndex),
            RequestVoteResponse{term, granted, logOk} =>
                write!(f.buf, "RequestVoteResponse(term: {}, granted: {}, logOk: {})",
                       term, granted, logOk),
            AppendEntriesRequest{term, seqno} =>
                write!(f.buf, "AppendEntriesRequest(term: {}, seqno: {})",
                       term, seqno),
            AppendEntriesResponse{term, seqno} =>
                write!(f.buf, "AppendEntriesResponse(term: {}, seqno: {})",
                       term, seqno),
        }
    }
}



enum ServerState {
    Follower  {
        timer: Time,
        last_heartbeat: Time,
    },
    Candidate {
        timer: Time,
        votes: HashSet<ServerID>,
        /**
         * If true, when the timer fires, the candidate should start a new election.
         * If false, when the timer fires, set a new timer.
         * Always true for submission algo, but sometimes false in hesitant, hesitant2.
         */
        should_retry: bool,
        pre: bool,
        max_term: Term,
        endorsements: HashSet<ServerID>,
    },
    Leader {
        timer: Time,
        heartbeat_seqno: uint,
        acks: HashSet<ServerID>,
        start_time: Time,
    },
}

impl ServerState {
    pub fn to_char(&self) -> char {
        match *self {
            Follower {..}  => 'F',
            Candidate {..} => 'C',
            Leader {..}    => 'L',
        }
    }
}

impl fmt::Default for HashSet<ServerID> {
    fn fmt(set: &HashSet<ServerID>, f: &mut fmt::Formatter) {
        write!(f.buf, "\\{");
        let mut sorted = set.iter().to_owned_vec();
        extra::sort::tim_sort(sorted);
        for s in sorted.iter() {
            write!(f.buf, "{}", **s);
        }
        write!(f.buf, "\\}");
    }
}

impl fmt::Default for Configuration {
    fn fmt(config: &Configuration, f: &mut fmt::Formatter) {
        write!(f.buf, "[");
        for c in config.iter() {
            write!(f.buf, "{}", *c);
        }
        write!(f.buf, "]");
    }
}

impl fmt::Default for ServerState {
    fn fmt(state: &ServerState, f: &mut fmt::Formatter) {
        match *state {
            Follower{timer, last_heartbeat} => {
                write!(f.buf, "Follower(timer: {}, last_heartbeat: {})",
                       timer, last_heartbeat)
            },
            Candidate{timer, votes: ref votes, should_retry, pre, max_term, endorsements: ref endorsements} => {
                write!(f.buf, "Candidate(timer: {}, votes: {}, should_retry: {}, pre: {}, max_term: {}, endorsements: {})",
                       timer, *votes, should_retry, pre, max_term, *endorsements)
            },
            Leader{timer, heartbeat_seqno, acks: ref acks, start_time} => {
                write!(f.buf, "Leader(timer: {}, seqno: {}, acks: {}, start_time: {})",
                       timer, heartbeat_seqno, *acks, start_time)
            },
        }
    }
}

struct LogEntry {
    term: Term,
    // command: ~str,
}

struct Configuration(~[HashSet<ServerID>]);

impl Configuration {
    fn is_quorum(&self, servers: &HashSet<ServerID>) -> bool {
        for c in self.iter() {
            if c.intersection(servers).len() <= c.len() / 2 {
                return false;
            }
        }
        return true;
    }
    fn can_quorum_without(&self, servers: &HashSet<ServerID>) -> bool {
        for c in self.iter() {
            if c.difference(servers).len() <= c.len() / 2 {
                return false;
            }
        }
        return true;
    }
}

struct Server {
    id: ServerID,
    config: Configuration,
    algorithm: ~str,
    term: Term,
    state: ServerState,
    vote: Option<ServerID>,
    lastLogIndex: Index,
    rejections: HashSet<ServerID>,
}

impl fmt::Default for Server {
    fn fmt(server: &Server, f: &mut fmt::Formatter) {
        write!(f.buf, "Server(id: {}, term: {}, log: {}, vote: {}, state: {}, rejections: {})",
               server.id,
               server.term,
               server.lastLogIndex,
               server.vote,
               server.state,
               server.rejections)
    }
}


impl Server {
    pub fn new(id : ServerID, config: Configuration, env: &Environment,
               algorithm: &str) -> Server {
        match algorithm {
            "nograntnobump" |
            "stalelognobump" |
            "hesitant"      |
            "hesitant2"     |
            "phesitant"     |
            "zookeeper"     |
            "zookeeper2"    |
            "lease"         |
            "submission"    => {},
            _ => fail!("Unknown algorithm: {}", algorithm)
        };
        Server {
            id: id,
            config: config,
            algorithm: algorithm.into_owned(),
            term: Term(0),
            state: Follower {
              timer: env.make_time(ELECTION_TIMEOUT),
              last_heartbeat: Time(0),
            },
            lastLogIndex: Index(0),
            vote: None,
            rejections: HashSet::new(),
        }
    }
    fn start_new_election(&mut self, env: &mut Environment, forceTerm: Option<Term>) {
        if self.algorithm == ~"phesitant" {
            if (!self.config.can_quorum_without(&self.rejections)) {
                match self.state {
                    Follower { timer: ref mut timer, .. } |
                    Candidate { timer: ref mut timer, .. } => {
                        if *timer <= env.clock {
                            *timer = env.make_time(ELECTION_TIMEOUT);
                        }
                    },
                    _ => fail!("Start new election from Leader?"),
                }
                return;
            }
        }
        let pre = forceTerm.is_none() && match self.algorithm {
            ~"zookeeper"  => true,
            ~"zookeeper2" => true,
            _ => false,
        };
        if !pre {
            self.term = match forceTerm {
                Some(t) if t > self.term => Term(*t + 1),
                _                        => Term(*self.term + 1),
            };
            self.vote = None;
        }
        self.state = Candidate {
            timer: env.make_time(ELECTION_TIMEOUT),
            votes: newHashSet([self.id]),
            should_retry: match self.algorithm {
                ~"hesitant"  => false,
                ~"hesitant2" => false,
                ~"phesitant" => false,
                _ => true,
            },
            pre: pre,
            max_term: Term(0),
            endorsements: newHashSet([self.id]),
        };
        self.vote = Some(self.id);
        env.multicast(self.id, &self.config, &RequestVoteRequest {
            term: self.term,
            lastLogIndex: self.lastLogIndex,
        });
        self.try_become_leader(env);
    }
    pub fn tick(&mut self, env: &mut Environment) {
        let mut need_step_down = false;

        match self.state {
            Follower  {timer, ..} => {
                if timer <= env.clock {
                    self.start_new_election(env, None)
                }
            }
            Candidate {timer, should_retry, ..}
                if timer <= env.clock && should_retry => {
                    self.start_new_election(env, None);
            },
            Candidate {timer: ref mut timer, should_retry, ..}
                if *timer <= env.clock && !should_retry => {
                    *timer = env.make_time(ELECTION_TIMEOUT);
            },
            Candidate {..} => {},
            Leader {timer: ref mut timer, heartbeat_seqno: ref mut heartbeat_seqno, acks: ref mut acks, ..} => {
                if *timer <= env.clock {
                    if (self.config.is_quorum(acks)) {
                        *heartbeat_seqno += 1;
                        acks.clear();
                        acks.insert(self.id);
                        *timer = env.make_time(HEARTBEAT_TIMEOUT);
                        env.multicast(self.id, &self.config, &AppendEntriesRequest {
                            term: self.term,
                            seqno: *heartbeat_seqno,
                        });
                    } else {
                        // TODO: this is probably too agressive, should only
                        // step down after election timeout
                        need_step_down = true;
                    }
                }
            },
        }
        if need_step_down {
            println!("Failed to maintain leadership: {}", *self);
            self.step_down(env, self.term)
        }
    }

    pub fn next_tick(&self) -> Time {
        let mut r = NEVER;
        match self.state {
            Follower { timer, .. }  => r = std::cmp::min(r, timer),
            Candidate { timer, .. } => r = std::cmp::min(r, timer),
            Leader { timer, .. }    => r = std::cmp::min(r, timer),
        }
        return r;
    }

    fn step_down(&mut self, env: &Environment, term: Term) {
        let (t, lh) = match self.state {
            Follower { timer, last_heartbeat } => (timer, last_heartbeat),
            Candidate { timer, .. } => (timer, Time(0)),
            Leader{..} => (env.make_time(ELECTION_TIMEOUT), env.clock),
        };
        self.term = term;
        self.vote = None;
        self.state = Follower {
            timer: t,
            last_heartbeat: lh,
        };
    }

    fn try_become_leader(&mut self, env: &mut Environment) {
        let become_leader = match self.state {
            Follower {..} => false,
            Candidate {votes: ref votes, ..} => self.config.is_quorum(votes),
            Leader {..} => false,
        };
        if become_leader {
            self.state = Leader {
                timer: env.make_time(HEARTBEAT_TIMEOUT),
                heartbeat_seqno: 0,
                acks: newHashSet([self.id]),
                start_time: env.clock,
            };
            env.multicast(self.id, &self.config, &AppendEntriesRequest {
                term: self.term,
                seqno: 0,
            });
        }
    }


    pub fn stable_leader_start_time(&self, stable_req: uint) -> Option<Time> {
        match self.state {
            Leader {heartbeat_seqno, start_time, ..} => {
                if heartbeat_seqno > stable_req {
                    Some(start_time)
                } else {
                    None
                }
            },
            _ => None,
        }
    }


    pub fn handle(&mut self, env: &mut Environment, msg: &Message) {
        match msg.body {
            RequestVoteRequest {term, lastLogIndex} => {
                let reply = |granted| {
                    env.reply(msg, &RequestVoteResponse {
                        // use max here for nograntnobump algorithm
                        term: std::cmp::max(term, self.term),
                        granted: granted,
                        logOk: lastLogIndex >= self.lastLogIndex,
                    });
                };
                if term < self.term {
                    reply(TERM_STALE);
                } else {
                    if self.algorithm == ~"lease" {
                      if match self.state {
                        Follower { last_heartbeat, .. } =>
                          last_heartbeat + ELECTION_TIMEOUT.first() > env.clock,
                        Candidate { .. } => false,
                        Leader { .. } => true,
                      } {
                        reply(LEASE);
                        return;
                      }
                    }
                    if term > self.term {
                        match self.algorithm {
                            ~"nograntnobump"  => {},
                            ~"stalelognobump" => {},
                            _ => self.step_down(env, term)
                        }
                    }
                    // careful ordering to support algorithm hesitant:
                    // reply with LOG_STALE first
                    if lastLogIndex < self.lastLogIndex {
                        reply(LOG_STALE);
                    } else {
                        if self.algorithm == ~"stalelognobump" && term > self.term {
                            self.step_down(env, term);
                        }
                        match self.vote {
                            None => {
                                if self.algorithm == ~"nograntnobump" && term > self.term {
                                    self.step_down(env, term);
                                }
                                self.vote = Some(msg.from);
                                match self.state {
                                    Follower { timer: ref mut timer, .. } =>
                                        *timer = env.make_time(ELECTION_TIMEOUT),
                                    _ => {},
                                }
                                reply(GRANTED);
                            },
                            Some(c) => {
                                reply(if c == msg.from { GRANTED } else { VOTED });
                            },
                        }
                    }
                }
            },
            RequestVoteResponse {term, granted, logOk} => {
                let msg_term = term;
                let term = match self.state {
                    Candidate {pre, ..} => {
                        if (term > self.term &&
                            pre &&
                            self.algorithm == ~"zookeeper2") {
                            self.term
                        } else {
                            term
                        }
                    },
                    _ => term,
                };
                if term == self.term {
                    let mut forceNewElectionTerm = None;
                    match self.state {
                        Follower {..} => {},
                        Candidate {votes: ref mut votes,
                                   should_retry: ref mut should_retry,
                                   pre: ref mut pre,
                                   max_term: ref mut max_term,
                                   endorsements: ref mut endorsements,
                                   ..} => {
                            if *pre {
                                *max_term = std::cmp::max(*max_term, msg_term);
                                if logOk {
                                    endorsements.insert(msg.from);
                                    if self.config.is_quorum(endorsements) {
                                        if self.algorithm == ~"zookeeper2" {
                                            forceNewElectionTerm = Some(*max_term);
                                        } else {
                                            forceNewElectionTerm = Some(Term(0));
                                        }
                                    }
                                }
                            } else {
                                match granted {
                                    GRANTED => {
                                        votes.insert(msg.from);
                                    },
                                    TERM_STALE | VOTED | LEASE => {
                                        endorsements.insert(msg.from);
                                        if self.algorithm == ~"hesitant" {
                                            *should_retry = true;
                                        }
                                        if self.algorithm == ~"hesitant2" {
                                            if self.config.is_quorum(endorsements) {
                                                *should_retry = true;
                                            }
                                        }
                                    },
                                    LOG_STALE => {
                                        self.rejections.insert(msg.from);
                                    },
                                }
                            }
                        },
                        Leader {..} => {},
                    }
                    match forceNewElectionTerm {
                        Some(t) => self.start_new_election(env, Some(t)),
                        None => {},
                    };
                    self.try_become_leader(env)
                } else if term > self.term {
                    self.step_down(env, term);
                }
            },
            AppendEntriesRequest {term, seqno} => {
                if term == self.term {
                    self.state = Follower {
                        timer: env.make_time(ELECTION_TIMEOUT),
                        last_heartbeat: env.clock,
                    };
                } else if term > self.term {
                    self.step_down(env, term);
                }
                env.reply(msg, &AppendEntriesResponse {
                    term: self.term,
                    seqno: seqno,
                });
            },
            AppendEntriesResponse {term, seqno} => {
                if term == self.term {
                    match self.state {
                        Follower {..} | Candidate {..} => {},
                        Leader { heartbeat_seqno, acks: ref mut acks, .. } => {
                            if seqno == heartbeat_seqno {
                                acks.insert(msg.from);
                            }
                        },
                    }
                } else if term > self.term {
                    self.step_down(env, term)
                }
            },
        }
    }

} // Server impl

