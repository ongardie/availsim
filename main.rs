#[feature(globs)];
#[feature(struct_variant)];
#[feature(macro_rules)];
extern mod std;
extern mod extra;
use extra::getopts;
use basics::*;
use policies::TimingPolicy;
use std::io::File;

mod basics;
mod policies;
mod raft;
mod sim;


/* features to support selectmatch */

/* SelectBox is a wrapper to allow calling select() on pointers */
struct SelectBox<'self> {
    inner: &'self mut std::select::Select,
}

impl<'self> std::rt::shouldnt_be_public::SelectInner for SelectBox<'self> {
    fn optimistic_check(&mut self) -> bool {
        self.inner.optimistic_check()
    }
    fn block_on(&mut self, sched: &mut std::rt::sched::Scheduler, task: std::rt::BlockedTask) -> bool {
        self.inner.block_on(sched, task)
    }
    fn unblock_from(&mut self) -> bool {
        self.inner.unblock_from()
    }
}

impl<'self> std::select::Select for SelectBox<'self> {
}

/* recursive helper for selectmatch macro */
macro_rules! selectmatch_rec(
    ($index:expr,
     $port:expr => $action:expr,
     $( $port_rest:expr => $action_rest:expr ),+) => (
        if $index == 0 {
            $action
        } else {
            $index -= 1;
            selectmatch_rec!($index, $( $port_rest => $action_rest ),*)
        }
    );
    ($index:expr, $port:expr => $action:expr) => (
        if $index == 0 {
            $action
        } else {
            fail!("Bogus return value from select()");
        }
    );
)

/* bearable syntax for calling select with SelectBox */
macro_rules! selectmatch(
     ($( $port:expr => $action:expr ),+) => (
        do 1.times {
            let mut selectmatch_i : uint = std::select::select([$( SelectBox{ inner: &mut $port as &mut std::select::Select } ),+]);
            selectmatch_rec!(selectmatch_i, $( $port => $action ),+)
        }
    );
)

/* end selectmatch */


fn main() {
    let args = std::os::args();
    let opts = [
        getopts::optflag("h"),
        getopts::optflag("help"),
        getopts::optopt("logs"),
        getopts::optopt("samples"),
        getopts::optopt("servers"),
        getopts::optopt("tasks"),
        getopts::optopt("timing"),
    ];
    let matches = match getopts::getopts(args.tail(), opts) {
        Ok(m) => { m },
        Err(f) => { fail!(f.to_err_msg()) },
    };
    let usage = || {
        println!("Usage: {} [options]", args[0]);
        println!("-h, --help       Print this help message");
        println!("--logs=POLICY    Log length policy (default same)");
        println!("--samples=N      Number of simulations (default 10,000)");
        println!("--servers=N      Simulate cluster with N servers");
        println!("--tasks=N        Number of parallel jobs (default {})",
                 std::rt::default_sched_threads() - 1);
        println!("--timing=POLICY  Network timing policy (default LAN)");
    };
    if matches.opt_present("h") || matches.opt_present("help") {
        usage();
        return;
    }
    if !matches.free.is_empty() {
        usage();
        fail!(format!("Extra arguments: {}", matches.free.len()));
    }
    let log_length : ~str = match matches.opt_str("logs") {
        Some(s) => { s },
        None => { ~"same" },
    };
    let num_samples : uint = match matches.opt_str("samples") {
        Some(s) => { match std::from_str::from_str(s) {
            Some(i) => { i },
            None => {
                usage();
                fail!(format!("Couldn't parse number of samples from '{}'", s));
            },
        }},
        None => { 10000 },
    };
    let num_servers : uint = match matches.opt_str("servers") {
        Some(s) => { match std::from_str::from_str(s) {
            Some(i) => { i },
            None => {
                usage();
                fail!(format!("Couldn't parse number of servers from '{}'", s));
            },
        }},
        None => { 5 },
    };
    let num_tasks : uint = match matches.opt_str("tasks") {
        Some(s) => { match std::from_str::from_str(s) {
            Some(i) => { i },
            None => {
                usage();
                fail!(format!("Couldn't parse number of tasks from '{}'", s));
            },
        }},
        None => { std::rt::default_sched_threads() - 1 },
    };
    let timing : ~str = match matches.opt_str("timing") {
        Some(s) => { s },
        None => { ~"LAN" },
    };

    let mut meta : ~[(~str, ~str)] = ~[];
    meta.push((~"log_length", log_length.clone()));
    meta.push((~"servers",    format!("{}", num_servers)));
    meta.push((~"tasks",      format!("{}", num_tasks)));
    meta.push((~"trials",     format!("{}", num_samples)));
    meta.push((~"timing",     timing.clone()));

    println!("Log length: {}", log_length)
    println!("Servers:    {}", num_servers)
    println!("Tasks:      {}", num_tasks)
    println!("Trials:     {}", num_samples)
    println!("Timing:     {}", timing)

    let start_ns = extra::time::precise_time_ns();

    let mut signals = std::io::signal::Listener::new();
    signals.register(std::io::signal::Interrupt);

    let samples = if num_tasks <= 1 {
        run_task(num_samples, num_servers, timing, log_length)
        // TODO: ctrl-c?
    } else {
        let (mut port, chan): (std::comm::Port<~[Time]>, std::comm::Chan<~[Time]>) = stream();
        let chan = std::comm::SharedChan::new(chan);
        for tid in range(0, num_tasks) {
            let child_chan = chan.clone();
            let child_log_length = log_length.clone();
            let child_timing = timing.clone();
            do spawn || {
                let n = if tid == 0 {
                    // get the odd one left over
                    num_samples - (num_samples / num_tasks) * (num_tasks - 1)
                } else {
                    num_samples / num_tasks
                };
                child_chan.send(run_task(n, num_servers, child_timing, child_log_length));
            }
        }

        let mut samples = ~[];
        samples.reserve(num_samples);
        let mut tasks_outstanding = num_tasks;
        while tasks_outstanding > 0 {
            //let (p1, c1): (std::rt::comm::Port<std::rt::io::signal::Signum>, std::rt::comm::Chan<std::rt::io::signal::Signum>) = std::rt::comm::stream();
            //let (p2, c2): (std::rt::comm::Port<~[uint]>, std::rt::comm::Chan<~[uint]>) = std::rt::comm::stream();
            //match std::select::select([p1 as &Select, p2 as &Select]) {

            selectmatch!(
                signals.port.x => {
                    match signals.port.recv() {
                        std::io::signal::Interrupt => {
                            println!("Should exit now");
                        },
                        _ => fail!("Unexpected signal"),
                    }
                },
                port.x => {
                    println!("Task completed");
                    tasks_outstanding -= 1;
                    samples.push_all(port.recv());
                }
            )
        }
        
        //do num_tasks.times {
        //    samples.push_all(port.recv());
        //}
        extra::sort::tim_sort(samples);
        samples
    };

    let end_ns = extra::time::precise_time_ns();
    let elapsed_ns = end_ns - start_ns;
    meta.push((~"wall",  format!("{}", elapsed_ns as f64 / 1e9)));

    println!("Min:     {} ticks", samples[0]);
    println!("Median:  {} ticks", samples[samples.len()/2]);
    println!("Max:     {} ticks", samples[samples.len()-1]);
    println!("Wall:    {:.2f} s",     elapsed_ns as f64 / 1e9);

    let metaf = &mut File::create(&Path::new("meta.csv")).unwrap() as &mut Writer;
    for &(ref k, ref _v) in meta.iter() {
        write!(metaf, "{}, ", *k);
    }
    write!(metaf, "\n");
    for &(ref _k, ref v) in meta.iter() {
        write!(metaf, "{}, ", *v);
    }
    write!(metaf, "\n");

    let runf = &mut File::create(&Path::new("samples.csv")).unwrap() as &mut Writer;
    writeln!(runf, "election_time");
    for sample in samples.iter() {
        writeln!(runf, "{}", *sample);
    }
}

fn run_task(n: uint, num_servers: uint, timing: &str, log_length: &str) -> ~[Time] {
    let mut samples = ~[];
    samples.reserve(n);
    do n.times {
        // TODO: may be better to reuse these timing policies.
        let p = policies::make(timing);
        let sample = sim::simulate(num_servers, p, log_length);
        samples.push(sample);
    }
    extra::sort::tim_sort(samples);
    return samples;
}
