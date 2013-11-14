require("ggplot2")
require("gridExtra")
require("scales")
meta = read.csv('meta.csv')
run = read.csv('samples.csv')
print(meta)
print(head(run))

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

title <- with(meta,
            sprintf('%s / logs %s / %d servers / %s trials',
                    timing, log_length, servers,
                    format(trials, big.mark=',')))

g <- {}

g$johncdf <- ggplot(run) + gtheme +
       stat_ecdf(aes(x=election_time)) +
       scale_y_continuous(breaks=c(0, .9, .99, .999, .9999, .99999, .999999),
                          trans=reverselog_trans(10)) +
       expand_limits(x=c(0, 1000)) +
       xlab('Election Time') +
       ylab('Cumulative Fraction') +
       ggtitle(title)

g$cdf <- ggplot(run) + gtheme +
       stat_ecdf(aes(x=election_time)) +
       coord_cartesian(x=c(0, 1000)) +
       xlab('Election Time') +
       ylab('Cumulative Fraction') +
       ggtitle(title)

ggsave(plot=arrangeGrob(g$cdf, g$johncdf, nrow=1),
        filename='Rplots.svg',
        width=7, height=3.5)
