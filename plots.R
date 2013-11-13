require("ggplot2")
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

johncdf_yscale = scale_y_continuous(
                    breaks=c(0, .9, .99, .999, .9999, .99999, .999999),
                    trans=reverselog_trans(10))

print(ggplot(run) +
       stat_ecdf(aes(x=election_time)) +
       johncdf_yscale +
       expand_limits(x=c(0, 1000)) +
       #coord_cartesian(x=c(0, 1000)) +
       xlab('Election Time') +
       ylab('Cumulative Fraction') +
       ggtitle(with(meta,
            sprintf('%s / %d servers / %s trials',
                    timing, servers, format(trials, big.mark=',')))))
