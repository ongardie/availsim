require("ggplot2")
meta = read.csv('meta.csv')
run = read.csv('samples.csv')
print(meta)
print(head(run))
print(ggplot(run) +
       stat_ecdf(aes(x=election_time)) +
       coord_cartesian(x=c(0, 1000)) +
       xlab('Election Time') +
       ylab('Cumulative Fraction') +
       ggtitle(with(meta,
            sprintf('%s / %d servers / %d trials',
                    timing, servers, trials))))
