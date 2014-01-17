availsim
========

Simulator for Raft's leader election algorithm and a bunch of alternatives.

Used to produce a bunch of graphs for my thesis. See my thesis for background/more info.

The code needs a lot of cleanup. It's embarrassing, but I feel obligated to release it in case someone wants to reproduce/extend my results.

The core simulator is written in Rust v0.9. I'd suggest getting that exact version if you want it to build.

There's also an interactive front-end written in node-webkit that lets your run simulations and see the results right away. I used v0.4.2, but this one should be less picky about the exact version.

All the graphs are created with R and ggplot2, so you'll need those as well to use the front-end.
