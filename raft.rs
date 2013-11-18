#[feature(globs)];
#[feature(struct_variant)];
extern mod std;
extern mod extra;

use basics::*;
use std::fmt;
use sim::Environment;
use std::hashmap::HashSet;

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

#[deriving(Eq)]
enum VoteGranted {
    GRANTED,
    TERM_STALE,
    LOG_STALE,
    VOTED,
}

impl fmt::Default for VoteGranted {
    fn fmt(g: &VoteGranted, f: &mut fmt::Formatter) {
        write!(f.buf, "{}", match *g {
            GRANTED    => "GRANTED",
            TERM_STALE => "TERM_STALE",
            LOG_STALE  => "LOG_STALE",
            VOTED      => "VOTED",
        });
    }
}


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
    },
    Candidate {
        timer: Time,
        votes: HashSet<ServerID>,
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
            Follower{timer} => {
                write!(f.buf, "Follower(timer: {})",
                       timer)
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
            if c.intersection_iter(servers).len() <= c.len() / 2 {
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
}

impl fmt::Default for Server {
    fn fmt(server: &Server, f: &mut fmt::Formatter) {
        write!(f.buf, "Server(id: {}, term: {}, log: {}, vote: {}, state: {}, config: {})",
               server.id,
               server.term,
               server.lastLogIndex,
               server.vote,
               server.state,
               server.config)
    }
}


impl Server {
    pub fn new(id : ServerID, config: Configuration, env: &Environment,
               algorithm: &str) -> Server {
        match algorithm {
            "nograntnobump" |
            "hesitant"      |
            "hesitant2"     |
            "zookeeper"     |
            "zookeeper2"    |
            "submission"    => {},
            _ => fail!("Unknown algorithm: {}", algorithm)
        };
        Server {
            id: id,
            config: config,
            algorithm: algorithm.into_owned(),
            term: Term(0),
            state: Follower { timer: env.make_time(150, 299) },
            lastLogIndex: Index(0),
            vote: None,
        }
    }
    fn start_new_election(&mut self, env: &mut Environment, forceTerm: Option<Term>) {
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
            timer: env.make_time(150, 299),
            votes: newHashSet([self.id]),
            should_retry: match self.algorithm {
                ~"hesitant"  => false,
                ~"hesitant2" => false,
                _ => true,
            },
            pre: pre,
            max_term: Term(0),
            endorsements: newHashSet([self.id]),
        };
        env.multicast(self.id, &self.config, &RequestVoteRequest {
            term: self.term,
            lastLogIndex: self.lastLogIndex,
        });
        self.try_become_leader(env);
    }
    pub fn tick(&mut self, env: &mut Environment) {
        let mut need_step_down = false;

        match self.state {
            Follower  {timer, _} => {
                if timer <= env.clock {
                    self.start_new_election(env, None)
                }
            }
            Candidate {timer, should_retry, _} => {
                if timer <= env.clock && should_retry {
                    self.start_new_election(env, None)
                }
            },
            Leader {timer: ref mut timer, heartbeat_seqno: ref mut heartbeat_seqno, acks: ref mut acks, _} => {
                if *timer <= env.clock {
                    if (self.config.is_quorum(acks)) {
                        *heartbeat_seqno += 1;
                        acks.clear();
                        acks.insert(self.id);
                        *timer = env.make_time(75, 75);
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
    fn step_down(&mut self, env: &Environment, term: Term) {
        let t = match self.state {
            Follower { timer, _ } => timer,
            Candidate { timer, _ } => timer,
            Leader{_} => env.make_time(150, 299),
        };
        self.term = term;
        self.vote = None;
        self.state = Follower {
            timer: t,
        };
    }

    fn try_become_leader(&mut self, env: &mut Environment) {
        let become_leader = match self.state {
            Follower {_} => false,
            Candidate {votes: ref votes, _} => self.config.is_quorum(votes),
            Leader {_} => false,
        };
        if become_leader {
            self.state = Leader {
                timer: env.make_time(75, 75),
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


    pub fn stable_leader_start_time(&self) -> Option<Time> {
        match self.state {
            Leader {heartbeat_seqno, start_time, _} if heartbeat_seqno > 4 => Some(start_time),
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
                    if self.algorithm != ~"nograntnobump" && term > self.term {
                        self.step_down(env, term);
                    }
                    // careful ordering to support algorithm hesitant:
                    // reply with LOG_STALE first
                    if lastLogIndex < self.lastLogIndex {
                        reply(LOG_STALE);
                    } else {
                        match self.vote {
                            None => {
                                if self.algorithm == ~"nograntnobump" && term > self.term {
                                    self.step_down(env, term);
                                }
                                self.vote = Some(msg.from);
                                match self.state {
                                    Follower { timer: ref mut timer, _ } =>
                                        *timer = env.make_time(150, 299),
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
                    Candidate {pre, _} => {
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
                        Follower {_} => {},
                        Candidate {votes: ref mut votes,
                                   should_retry: ref mut should_retry,
                                   pre: ref mut pre,
                                   max_term: ref mut max_term,
                                   endorsements: ref mut endorsements,
                                   _} => {
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
                                    TERM_STALE | VOTED => {
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
                                    LOG_STALE => {},
                                }
                            }
                        },
                        Leader {_} => {},
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
                        timer: env.make_time(150, 299),
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
                        Follower {_} | Candidate {_} => {},
                        Leader { heartbeat_seqno, acks: ref mut acks, _ } => {
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

