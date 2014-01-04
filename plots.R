require("ggplot2")
require("gridExtra")
require("scales")

reverselog_trans <- function(base = exp(1)) {
    trans <- function(x) 1-log(1-x, base)
    inv   <- function(y) 1-base^(1-y)
    trans_new(paste0("reverselog-", format(base)),
              trans, inv,
              log_breaks(base = base),
              domain = c(1e-99, Inf))
}

gtheme <- theme_bw(base_size = 12, base_family = "") +
          theme(plot.margin=unit(c(.2,0,0,0), 'cm'),
                legend.margin=unit(0, 'cm'))
                #axis.title=element_text(size = rel(.8)),
                #plot.title=element_text(size = rel(.8)))

cdf <- function() {

    meta = read.csv('meta.csv')
    run = read.csv('samples.csv')

    title <- with(meta,
                sprintf('%s / %s / logs %s / terms %s /\ncluster %s / %d heartbeats / %s trials',
                        algorithm, timing, log_length, terms, cluster, heartbeats,
                        format(trials, big.mark=',')))

    g <- {}

    etmean <- mean(run$election_time) / 1e3
    etcdf <- ecdf(run$election_time / 1e3)

    g$johncdf <- ggplot(run) + gtheme +
           stat_ecdf(aes(x=election_time/1e3)) +
           scale_y_continuous(breaks=c(0, .9, .99, .999, .9999, .99999, .999999),
                              trans=reverselog_trans(10)) +
           expand_limits(x=c(0, 1000)) +
           geom_point(x=etmean, y=1-log(1-etcdf(etmean), 10)) +
           xlab('Election Time (ms)') +
           ylab('Cumulative Fraction') +
           geom_vline(xintercept = 1000)


    g$cdf <- ggplot(run) + gtheme +
           stat_ecdf(aes(x=election_time/1e3)) +
           coord_cartesian(x=c(0, 1050)) +
           scale_x_continuous(breaks=seq(0, 1050, 100)) +
           scale_y_continuous(breaks=seq(0, 1, .1)) +
           geom_point(x=etmean, y=etcdf(etmean)) +
           xlab('Election Time (ms)') +
           ylab('Cumulative Fraction')

    ggsave(plot=arrangeGrob(g$cdf, g$johncdf, nrow=1, main=title),
            filename='Rplots.svg',
            width=7, height=3.5)

}

# just used for lunch talk, sligtly different
timeline_plot <- function(run) {
    events = read.csv(sprintf('events%06d.csv', run))
    events$state = factor(events$state,
                          levels=c('F', 'C', 'L'))

    ggplot(events) + gtheme +
           geom_point(aes(x=time/1e3, y=server, color=state, shape=state),
                      size=2) +
           expand_limits(y=c(1,7)) +
           scale_x_continuous(breaks=function(l) { seq(0, l[2], 100)}) +
           scale_y_reverse(breaks=1:7) +
           geom_text(aes(x=time/1e3, y=server-.6, label=(term %% 10)), vjust=1, size=3) +
           xlab('Time (ms)') +
           ylab('Server')
}

timeline <- function(run) {
    events = read.csv(sprintf('events%06d.csv', run))
    events$state = factor(events$state,
                          levels=c('F', 'C', 'L'))

    g <- {}

    g$timeline <- ggplot(events) + gtheme +
           geom_point(aes(x=time/1e3, y=server, color=state, shape=state),
                      size=3) +
           expand_limits(y=c(1,7)) +
           scale_x_continuous(breaks=function(l) { seq(0, l[2], 100)}) +
           scale_y_reverse(breaks=1:7) +
           geom_text(aes(x=time/1e3, y=server-.5, label=(term %% 10)), vjust=1) +
           xlab('Time (ms)') +
           ylab('Server')
    ggsave(plot=g$timeline,
           filename=sprintf('timeline%06d.svg', run),
           width=max(10, min(max(events$time / 100000), 49)), height=3.5)

}

if (exists('repl') && repl) {
    home <- getwd()
    while (TRUE) {
        cmd <- readline('cmd: ')
        eval(parse(text=cmd))
        setwd(home)
        write('--DONE--\n', '')
    }
} else {
    cdf()
}

