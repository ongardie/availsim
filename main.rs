#[feature(globs)];
#[feature(struct_variant)];
#[feature(macro_rules)];
extern mod std;
extern mod extra;
use extra::getopts;
use basics::*;
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
        {
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
        getopts::optopt("algorithm"),
        getopts::optopt("cluster"),
        getopts::optopt("heartbeats"),
        getopts::optopt("logs"),
        getopts::optopt("maxticks"),
        getopts::optopt("samples"),
        getopts::optopt("tasks"),
        getopts::optopt("terms"),
        getopts::optopt("timeout"),
        getopts::optopt("timing"),
        getopts::optopt("trace"),
    ];
    let matches = match getopts::getopts(args.tail(), opts) {
        Ok(m) => { m },
        Err(f) => { fail!(f.to_err_msg()) },
    };
    let usage = || {
        println!("Usage: {} [options]", args[0]);
        println!("--algorithm=ALGO Algorithm variant (default submission)");
        println!("--cluster=POLICY Type/size of cluster (default 5)");
        println!("-h, --help       Print this help message");
        println!("--heartbeats=N   Number of heartbeats till leader ");
        println!("                 considered stable (default 16)");
        println!("--logs=POLICY    Log length policy (default same)");
        println!("--maxticks=N     Number of simulation ticks after which ");
        println!("                 to stop a sample (default 5,000,000)");
        println!("--samples=N      Number of simulations (default 10,000)");
        println!("--tasks=N        Number of parallel jobs (default {})",
                 std::rt::default_sched_threads() - 1);
        println!("--terms=POLICY   Initial terms policy (default same)");
        println!("--timeout=MS     Milliseconds after which to stop");
        println!("                 (default 0 meaning infinity)");
        println!("--timing=POLICY  Network timing policy (default LAN)");
        println!("--trace=N         Dump N simulation traces to files (default 0)");
    };
    if matches.opt_present("h") || matches.opt_present("help") {
        usage();
        return;
    }
    if !matches.free.is_empty() {
        usage();
        fail!(format!("Extra arguments: {}", matches.free.len()));
    }
    let algorithm : ~str = match matches.opt_str("algorithm") {
        Some(s) => { s },
        None => { ~"submission" },
    };
    let cluster : ~str = match matches.opt_str("cluster") {
        Some(s) => { s },
        None => { ~"5" },
    };
    let heartbeats : uint = match matches.opt_str("heartbeats") {
        Some(s) => { match std::from_str::from_str(s) {
            Some(i) => { i },
            None => {
                usage();
                fail!(format!("Couldn't parse number of heartbeats from '{}'", s));
            },
        }},
        None => { 16 },
    };
    let log_length : ~str = match matches.opt_str("logs") {
        Some(s) => { s },
        None => { ~"same" },
    };
    let max_ticks : Time = match matches.opt_str("maxticks") {
        Some(s) => { match std::from_str::from_str(s) {
            Some(i) => { Time(i) },
            None => {
                usage();
                fail!(format!("Couldn't parse max number of ticks from '{}'", s));
            },
        }},
        None => { Time(5000000) },
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
    let terms : ~str = match matches.opt_str("terms") {
        Some(s) => { s },
        None => { ~"same" },
    };
    let timeout : uint = match matches.opt_str("timeout") {
        Some(s) => { match std::from_str::from_str(s) {
            Some(i) => { i },
            None => {
                usage();
                fail!(format!("Couldn't parse timeout from '{}'", s));
            },
        }},
        None => { 0 },
    };
    let timeout = if timeout == 0 {
        !0
    } else {
        timeout
    };
    let timing : ~str = match matches.opt_str("timing") {
        Some(s) => { s },
        None => { ~"LAN" },
    };
    let trace : uint = match matches.opt_str("trace") {
        Some(s) => { match std::from_str::from_str(s) {
            Some(i) => { i },
            None => {
                usage();
                fail!(format!("Couldn't parse number of traces from '{}'", s));
            },
        }},
        None => { 0 },
    };

    let mut meta : ~[(~str, ~str)] = ~[];
    meta.push((~"algorithm",  algorithm.clone()));
    meta.push((~"log_length", log_length.clone()));
    meta.push((~"cluster",    cluster.clone()));
    meta.push((~"heartbeats", format!("{}", heartbeats)));
    meta.push((~"maxticks",   format!("{}", max_ticks)));
    meta.push((~"tasks",      format!("{}", num_tasks)));
    meta.push((~"terms",      terms.clone()));
    meta.push((~"timing",     timing.clone()));
    meta.push((~"trials_req", format!("{}", num_samples)));
    meta.push((~"timeout",    format!("{}", timeout)));

    println!("Algorithm:  {}", algorithm)
    println!("Cluster:    {}", cluster)
    println!("Heartbeats: {}", heartbeats)
    println!("Log length: {}", log_length)
    println!("Max ticks:  {}", max_ticks)
    println!("Tasks:      {}", num_tasks)
    println!("Terms:      {}", terms)
    println!("Timing:     {}", timing)
    println!("Req. trials:{}", num_samples)
    println!("Timeout:    {} ms", timeout)

    let start_ns = extra::time::precise_time_ns();

    let mut signals = std::io::signal::Listener::new();
    signals.register(std::io::signal::Interrupt);

    let sim_opts = sim::SimOpts {
       cluster: cluster,
       heartbeats: heartbeats,
       timing: timing,
       log_length: log_length,
       algorithm: algorithm,
       terms: terms,
       max_ticks: max_ticks,
       trace: trace,
    };
    let samples = if num_tasks <= 1 {
        run_task(std::iter::range_step(0, num_samples, 1), &sim_opts, &signals.port)
    } else {
        let (mut port, chan): (std::comm::Port<~[Sample]>, std::comm::Chan<~[Sample]>) = stream();
        let chan = std::comm::SharedChan::new(chan);
        let mut exit_chans : ~[std::comm::Chan<()>] = ~[];
        let exit = || {
            for c in exit_chans.iter() {
                c.send(());
            }
        };
        for tid in range(0, num_tasks) {
            let (exit_port, exit_chan): (std::comm::Port<()>, std::comm::Chan<()>) = stream();
            exit_chans.push(exit_chan);
            let child_chan = chan.clone();
            let child_opts = sim_opts.clone();
            do spawn {
                let runs = std::iter::range_step(tid, num_samples, num_tasks);
                child_chan.send(run_task(runs, &child_opts, &exit_port));
            }
        }

        let mut samples = ~[];
        samples.reserve(num_samples);
        let mut tasks_outstanding = num_tasks;
        let mut timeout_timer = std::io::timer::Timer::new().unwrap();
        // simulate a one_shot timer with a periodic timer and a boolean, since
        // I can't get the types to work otherwise
        let mut timeout_port = timeout_timer.periodic(timeout as u64);
        let mut timed_out = false;
        while tasks_outstanding > 0 {
            selectmatch!(
                signals.port.x => {
                    match signals.port.recv() {
                        std::io::signal::Interrupt => {
                            println!("\nCtrl-C: exiting");
                        },
                        _ => fail!("Unexpected signal"),
                    }
                },
                timeout_port.x => {
                    timeout_port.recv();
                    if !timed_out {
                        println!("Timeout");
                        timed_out = true;
                        exit();
                    }
                },
                port.x => {
                    tasks_outstanding -= 1;
                    samples.push_all(port.recv());
                }
            )
        }
        sort_samples(samples);
        samples
    };

    let end_ns = extra::time::precise_time_ns();
    let elapsed_ns = end_ns - start_ns;
    meta.push((~"wall",   format!("{}", elapsed_ns as f64 / 1e9)));
    meta.push((~"trials", format!("{}", samples.len())));
    meta.push((~"min",    format!("{}", samples[0].ticks)));
    meta.push((~"median", format!("{}", samples[samples.len()/2].ticks)));
    meta.push((~"max",    format!("{}", samples[samples.len()-1].ticks)));

    println!("Trials:     {}",       samples.len());
    println!("Min:        {} ticks", samples[0].ticks);
    println!("Median:     {} ticks", samples[samples.len()/2].ticks);
    println!("Max:        {} ticks", samples[samples.len()-1].ticks);
    println!("Wall:       {:.2f} s", elapsed_ns as f64 / 1e9);

    let metaf = &mut File::create(&Path::init("meta.csv")).unwrap() as &mut Writer;
    for &(ref k, ref _v) in meta.iter() {
        write!(metaf, "{}, ", *k);
    }
    write!(metaf, "\n");
    for &(ref _k, ref v) in meta.iter() {
        write!(metaf, "{}, ", *v);
    }
    write!(metaf, "\n");

    let runf = &mut File::create(&Path::init("samples.csv")).unwrap() as &mut Writer;
    let runf = &mut std::io::buffered::BufferedWriter::new(runf) as &mut Writer;
    writeln!(runf, "election_time,run");
    for sample in samples.iter() {
        writeln!(runf, "{},{}", sample.ticks, sample.run);
    }
}

#[deriving(Clone)]
struct Sample {
    run: uint,
    ticks: Time,
}

fn sort_samples(samples: &mut [Sample]) {
    extra::sort::quick_sort(samples, |x,y| x.ticks <= y.ticks);
}

fn run_task<T: Send>(mut runs: std::iter::RangeStep<uint>, opts: &sim::SimOpts, exit_port: &Port<T>) -> ~[Sample] {
    let mut samples = ~[];
    samples.reserve(runs.size_hint().first());
    for run in runs {
        if exit_port.peek() {
            return samples;
        }
        let sample = Sample {
            run: run,
            ticks: sim::simulate(run, opts),
        };
        samples.push(sample);
    }
    sort_samples(samples);
    return samples;
}
