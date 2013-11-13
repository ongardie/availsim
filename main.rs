#[feature(globs)];
#[feature(struct_variant)];
#[feature(macro_rules)];
extern mod std;
extern mod extra;
use extra::getopts;
use basics::*;
use policies::TimingPolicy;

mod basics;
mod policies;
mod raft;
mod sim;

fn main() {
    let args = std::os::args();
    let opts = [
        getopts::optflag("h"),
        getopts::optflag("help"),
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
        println!("--samples=N      Number of simulations (default 10,000)");
        println!("--servers=N      Simulate cluster with N servers");
        println!("--tasks=N        Number of parallel jobs (default {})",
                 std::rt::default_sched_threads() - 1);
        println!("--timing=POLICY  Number of timing policy (default LAN)");
    };
    if matches.opt_present("h") || matches.opt_present("help") {
        usage();
        return;
    }
    if !matches.free.is_empty() {
        usage();
        fail!(format!("Extra arguments: {}", matches.free.len()));
    }
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

    println!("Servers: {}", num_servers)
    println!("Tasks:   {}", num_tasks)
    println!("Trials:  {}", num_samples)
    println!("Timing:  {}", timing)

    let start_ns = extra::time::precise_time_ns();

    let samples = if num_tasks <= 1 {
        run_task(num_samples, num_servers, timing)
    } else {
        let (port, chan): (Port<~[Time]>, Chan<~[Time]>) = stream();
        let chan = std::comm::SharedChan::new(chan);
        for tid in range(0, num_tasks) {
            let child_chan = chan.clone();
            let child_timing = timing.clone();
            do spawn || {
                let n = if tid == 0 {
                    // get the odd one left over
                    num_samples - (num_samples / num_tasks) * (num_tasks - 1)
                } else {
                    num_samples / num_tasks
                };
                child_chan.send(run_task(n, num_servers, child_timing));
            }
        }

        let mut samples = ~[];
        samples.reserve(num_samples);
        do num_tasks.times {
            samples.push_all(port.recv());
        }
        extra::sort::tim_sort(samples);
        samples
    };

    let end_ns = extra::time::precise_time_ns();
    let elapsed_ns = end_ns - start_ns;

    println!("Min:     {} ticks", samples[0]);
    println!("Median:  {} ticks", samples[samples.len()/2]);
    println!("Max:     {} ticks", samples[samples.len()-1]);
    println!("Wall:    {:.2f} s",     elapsed_ns as f64 / 1e9);
}

fn run_task(n: uint, num_servers: uint, timing: &str) -> ~[Time] {
    let mut samples = ~[];
    samples.reserve(n);
    do n.times {
        // TODO: may be better to reuse these timing policies.
        let p = policies::make(timing);
        let sample = sim::simulate(num_servers, p);
        samples.push(sample);
    }
    extra::sort::tim_sort(samples);
    return samples;
}
