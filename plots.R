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
                legend.margin=unit(0, 'cm'),
                axis.title=element_text(size = rel(.8)),
                plot.title=element_text(size = rel(.8)))

cdf <- function() {

    meta = read.csv('meta.csv')
    run = read.csv('samples.csv')

    title <- with(meta,
                sprintf('%s / %s / logs %s / terms %s / cluster %s / %s trials',
                        algorithm, timing, log_length, terms, cluster,
                        format(trials, big.mark=',')))

    g <- {}

    g$johncdf <- ggplot(run) + gtheme +
           stat_ecdf(aes(x=election_time/1e6)) +
           scale_y_continuous(breaks=c(0, .9, .99, .999, .9999, .99999, .999999),
                              trans=reverselog_trans(10)) +
           expand_limits(x=c(0, 1)) +
           xlab('Election Time (s)') +
           ylab('Cumulative Fraction') +
           geom_vline(xintercept = 1)


    g$cdf <- ggplot(run) + gtheme +
           stat_ecdf(aes(x=election_time/1e6)) +
           coord_cartesian(x=c(0, 1)) +
           xlab('Election Time (s)') +
           ylab('Cumulative Fraction')

    ggsave(plot=arrangeGrob(g$cdf, g$johncdf, nrow=1, main=title),
            filename='Rplots.svg',
            width=7, height=3.5)

}

timeline <- function(run) {
    events = read.csv(sprintf('events%06d.csv', run))
    events$state = factor(events$state,
                          levels=c('F', 'C', 'L'))

    g <- {}

    g$timeline <- ggplot(events) + gtheme +
           geom_point(aes(x=time/1e3, y=server, color=state, shape=state),
                      size=3) +
           geom_text(aes(x=time/1e3, y=server+.1, label=term), vjust=0) +
           xlab('Time (ms)') +
           ylab('Server')
    ggsave(plot=g$timeline,
           filename=sprintf('timeline%06d.svg', run),
           width=7, height=3.5)

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

