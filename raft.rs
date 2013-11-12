#[feature(globs)];
use basics::*;
use std::fmt;
use sim::Environment;

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

pub enum MessageBody {
    RequestVoteRequest {
        term: Term,
        lastLogTerm: Term,
        lastLogIndex: Index,
    },
    RequestVoteResponse {
        term: Term,
        granted: bool,
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
            RequestVoteRequest{term, lastLogTerm, lastLogIndex} =>
                write!(f.buf, "RequestVoteRequest(term: {}, lastLogTerm: {}, lastLogIndex: {})",
                       term, lastLogTerm, lastLogIndex),
            RequestVoteResponse{term, granted} =>
                write!(f.buf, "RequestVoteResponse(term: {}, granted: {})",
                       term, granted),
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
    Follower  { timer: Time },
    Candidate { timer: Time, votes: uint },
    Leader    { timer: Time, heartbeat_seqno: uint, acks: uint, start_time: Time},
}


impl fmt::Default for ServerState {
    fn fmt(state: &ServerState, f: &mut fmt::Formatter) {
        match *state {
            Follower{timer} => {
                write!(f.buf, "Follower(timer: {})",
                       timer)
            },
            Candidate{timer, votes} => {
                write!(f.buf, "Candidate(timer: {}, votes: {})",
                       timer, votes)
            },
            Leader{timer, heartbeat_seqno, acks, start_time} => {
                write!(f.buf, "Leader(timer: {}, seqno: {}, acks: {}, start_time: {})",
                       timer, heartbeat_seqno, acks, start_time)
            },
        }
    }
}

struct LogEntry {
    term: Term,
    // command: ~str,
}

struct Log(~[LogEntry]);

impl Log {
    fn new() -> Log {
        Log(~[])
    }
}

struct Server {
    id: ServerID,
    peers: ~[ServerID],
    term: Term,
    state: ServerState,
    vote: Option<ServerID>,
    log: Log,
}

impl Server {
    pub fn new(id : ServerID, peers: ~[ServerID], env: &Environment) -> Server {
        Server {
            id: id,
            peers: peers,
            term: Term(0),
            state: Follower { timer: env.make_time(150, 299) },
            log: Log::new(),
            vote: None,
        }
    }
    pub fn tick(&mut self, env: &mut Environment) {
        let mut need_step_down = false;
        match self.state {
            Follower  {timer, _} |
            Candidate {timer, _} => {
                if timer <= env.clock {
                    self.term = Term(*self.term + 1);
                    self.vote = None;
                    self.state = Candidate {
                        timer: env.make_time(150, 299),
                        votes: 1,
                    };
                    env.multicast(self.id, self.peers, &RequestVoteRequest {
                        term: self.term,
                        lastLogTerm: Term(0),
                        lastLogIndex: Index(0),
                    });
                }
                self.try_become_leader(env);
            },
            Leader {timer: ref mut timer, heartbeat_seqno: ref mut heartbeat_seqno, acks: ref mut acks, _} => {
                if *timer <= env.clock {
                    if (*acks > (self.peers.len() + 1) / 2) {
                        *heartbeat_seqno += 1;
                        *acks = 0;
                        *timer = env.make_time(75, 75);
                        env.multicast(self.id, self.peers, &AppendEntriesRequest {
                            term: self.term,
                            seqno: *heartbeat_seqno,
                        });
                    } else {
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
        self.term = term;
        self.vote = None;
        self.state = Follower {
            timer: env.make_time(150, 299),
        };
    }

    fn try_become_leader(&mut self, env: &mut Environment) {
        match self.state {
            Follower {_} => {},
            Candidate {votes, _} => {
                if votes > (self.peers.len() + 1) / 2 {
                    self.state = Leader {
                        timer: env.make_time(75, 75),
                        heartbeat_seqno: 0,
                        acks: 1,
                        start_time: env.clock,
                    };
                    env.multicast(self.id, self.peers, &AppendEntriesRequest {
                        term: self.term,
                        seqno: 0,
                    });
                }
            },
            Leader {_} => {},
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
            RequestVoteRequest {term, _} => {
                let reply = |granted| {
                    env.reply(msg, &RequestVoteResponse {
                        term: self.term,
                        granted: granted,
                    });
                };
                if term < self.term {
                    reply(false);
                } else {
                    if term > self.term {
                        self.step_down(env, term);
                    }
                    match self.vote {
                        None => {
                            self.vote = Some(msg.from);
                            reply(true);
                        },
                        Some(c) => {
                            reply(c == msg.from);
                        },
                    }
                }
            },
            RequestVoteResponse {term, granted, _} => {
                if term == self.term && granted {
                    match self.state {
                        Follower {_} => {},
                        Candidate {votes: ref mut votes, _} => {
                            *votes += 1;
                        },
                        Leader {_} => {},
                    }
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
                                *acks += 1;
                            }
                        },
                    }
                }
            },
        }
    }

} // Server impl

impl fmt::Default for Server {
    fn fmt(server: &Server, f: &mut fmt::Formatter) {
        write!(f.buf, "Server {}: {}", *server.id, server.state)
    }
}

