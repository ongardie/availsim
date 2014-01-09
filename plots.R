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

earliest <- function(x, s, b, mint=100, maxt=200) {
    ifelse(x < mint + 2*b,
           0,
           ifelse(x > maxt + 2*b,
             1,
             1 - (1-(x-mint-2*b)/(maxt-mint))^s
           )
          )
}

summation <- function(lower, upper, fun) {
  s = 0
  for (i in lower:upper) {
    s = s + fun(i)
  }
  s
}

distance <- function(d, c, s) {
  ifelse(d < 0, 0, ifelse(d > 1, 1,
         summation(0, s-c+1, function(i) {
           choose(s, i) * d**(s-i) * (1-d)**i
         })
         ))
}

overall_sample <- function(s, b, b2, mint=100, maxt=200) {
  r <- 0
  while (T) {
    r <- r + mint + min(sample.int(maxt-mint, size=s)) + b2
    if (distance(b/(maxt-mint),3,s) < sample.int(1e9, size=1)/1e9)
      break
  }
  r
}

overall_cdf <- function(s, b, b2, mint=100, maxt=200) {
  data.frame(x=sapply(1:100000,
             function(x) { overall_sample(s=s, b=b, b2=b2) + b2 }))
}

make_title <- function(meta) {
    with(meta,
         sprintf('%s / %s / logs %s / terms %s / cluster %s / %d heartbeats / %s trials',
                 algorithm, timing, log_length, terms, cluster, heartbeats,
                 format(trials, big.mark=',')))
}

johncdfscalelab <-function(b) {
  breaks <- c(0, 1:9*10**-1, 1-9:1*10**-2, 1-9:1*10**-3, 1-9:1*10**-4)
  labels <- c()
  for (i in 1:length(breaks)) {
    labels <- append(labels, ifelse(i %% 9 == 1, breaks[i], ''))
  }
  b +
           #scale_y_continuous(breaks=c(0, .9, .99, .999, .9999, .99999, .999999),
           #                   trans=reverselog_trans(10)) +
           scale_y_continuous(breaks=breaks,
                              labels=labels,
                              trans=reverselog_trans(10)) +
           expand_limits(x=c(0, 1000)) +
           xlab('Election Time (ms)') +
           ylab('Cumulative Fraction') +
           geom_vline(xintercept = 1000)
}

cdfscalelab <- function(b) {
           b +
           coord_cartesian(x=c(0, 1060)) +
           scale_x_continuous(breaks=seq(0, 1060, 100),
                              labels=c(0, '', 200, '', 400, '', 600, '', 800, '', 1000)) +
           scale_y_continuous(breaks=seq(0, 1, .1), limits=c(0,1)) +
           xlab('Election Time (ms)') +
           ylab('Cumulative Fraction')
}

cdf <- function(thesis=F) {

    dir <- basename(getwd())

    meta <- read.csv('meta.csv')
    run <- read.csv('samples.csv')

    title <- make_title(meta)

    g <- {}

    etmean <- mean(run$election_time) / 1e3
    etcdf <- ecdf(run$election_time / 1e3)

    g$johncdf <- ggplot(run) + gtheme
    if (thesis & dir == 'submission-normal-RAMCloud') {
      g$johncdf <- g$johncdf +
           stat_function(fun=earliest, arg=list(s=meta$cluster, b=0), color='blue')
    }
    if (thesis & dir == 'submission-normal-LAN') {
      g$johncdf <- g$johncdf +
           stat_ecdf(data=overall_cdf(s=meta$cluster, b=3.5/2, b2=3.5), aes(x=x), color='blue')
    }
    if (thesis & dir == 'submission-normal-WAN') {
      g$johncdf <- g$johncdf +
           stat_ecdf(data=overall_cdf(s=meta$cluster, b=7.5, b2=15), aes(x=x), color='blue')
    }
    g$johncdf <- g$johncdf +
           stat_ecdf(aes(x=election_time/1e3))
    g$johncdf <- johncdfscalelab(g$johncdf) +
           geom_point(x=etmean, y=1-log(1-etcdf(etmean), 10))


    g$cdf <- ggplot(run) + gtheme
    if (thesis & dir == 'submission-normal-RAMCloud') {
      g$cdf <- g$cdf +
           stat_function(fun=earliest, arg=list(s=meta$cluster, b=0), color='blue')
    }
    if (thesis & dir == 'submission-normal-LAN') {
      g$cdf <- g$cdf +
           stat_ecdf(data=overall_cdf(s=meta$cluster, b=3.5/2, b2=3.5), aes(x=x), color='blue')
    }
    if (thesis & dir == 'submission-normal-WAN') {
      g$cdf <- g$cdf +
           stat_ecdf(data=overall_cdf(s=meta$cluster, b=7.5, b2=15), aes(x=x), color='blue')
    }
    g$cdf <- g$cdf +
           stat_ecdf(aes(x=election_time/1e3))
    g$cdf <- cdfscalelab(g$cdf) +
           geom_point(x=etmean, y=etcdf(etmean))

    ggsave(plot=arrangeGrob(g$cdf, g$johncdf, nrow=1, main=textGrob(title, gp=gpar(cex=.3))),
            filename='Rplots.svg',
            width=6, height=2)

}

g_legend <- function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

nolegend <- function(p) {
  p + theme(legend.position="none")
}

multicdf_helper <- function(dirs, labels, legendlabel, legendrows=3) {

    topdir <- getwd()
    meta <- data.frame()
    run <- data.frame()
    means <- data.frame()
    for (i in 1:length(dirs)) {
      setwd(dirs[i])
      dirmeta <- cbind(read.csv('meta.csv'), label=labels[i])
      dirrun <- cbind(read.csv('samples.csv'), label=labels[i])
      rbind(meta, dirmeta)->meta
      rbind(run, dirrun)->run

      dirmean <- mean(dirrun$election_time) / 1e3
      dirfraction <- ecdf(dirrun$election_time / 1e3)(dirmean)
      rbind(means, data.frame(etmean=dirmean,
                              fraction=dirfraction,
                              label=labels[i]))->means
      setwd(topdir)
    }

    #print(head(meta))
    #print(head(run))
    #print(head(means))


    title <- make_title(meta)

    g <- {}


    g$johncdf <- ggplot(run) + gtheme
    g$johncdf <- g$johncdf +
           stat_ecdf(aes(x=election_time/1e3, color=label))
    g$johncdf <- johncdfscalelab(g$johncdf) +
           geom_point(data=means, aes(x=etmean, y=fraction, color=label))


    g$cdf <- ggplot(run) + gtheme + scale_colour_discrete(legendlabel) +
             guides(col=guide_legend(nrow = legendrows))
    g$cdf <- g$cdf +
           stat_ecdf(aes(x=election_time/1e3, color=label))
    g$cdf <- cdfscalelab(g$cdf) +
           geom_point(data=means, aes(x=etmean, y=fraction, color=label))

    #ggsave(plot=arrangeGrob(g$cdf, g$johncdf, nrow=1, main=textGrob(title, gp=gpar(cex=.3))),
    #        filename='Rplots.svg',
    #        width=6, height=2)

    list(cdf=g$cdf, johncdf=g$johncdf, title=title)
}

multicdf <- function(dirs, labels, legendlabel, legendrows=3) {
    if (legendrows == 1) {
      heights <- c(5/6, 1/6)
    } else {
      heights <- c(4/6, 2/6)
    }
    g <- multicdf_helper(dirs, labels, legendlabel, legendrows)
    arrangeGrob(arrangeGrob(g$cdf + theme(legend.position="none"),
                            g$johncdf + theme(legend.position="none"),
                            nrow=1,
                            main=textGrob(g$title, gp=gpar(cex=.3))),
                g_legend(g$cdf),
                nrow=2,
                heights=heights)
}

multimulticdf <- function(top, bottom, heights) {
    arrangeGrob(
                arrangeGrob(nolegend(top$cdf),
                            nolegend(top$johncdf),
                            nrow=1,
                            main=textGrob(top$title, gp=gpar(cex=.3))),
                arrangeGrob(nolegend(bottom$cdf),
                            nolegend(bottom$johncdf),
                            nrow=1,
                            main=textGrob(bottom$title, gp=gpar(cex=.3))),
                g_legend(top$cdf),
                nrow=3,
                heights=heights)
}


# just used for lunch talk, sligtly different
timeline_plot <- function(run) {
    events = read.csv(sprintf('events%06d.csv', run))
    events$State = factor(events$state,
                          levels=c('F', 'C', 'L'),
                          labels=c('Follower', 'Candidate', 'Leader'))
    events <- cbind(events, servers=max(events$server))
    ggplot(events) + gtheme +
           geom_point(aes(x=time/1e3, y=server, color=State, shape=State),
                      size=3) +
           expand_limits(y=c(1,events$servers)) +
           scale_x_continuous(breaks=function(l) { seq(0, l[2], 100)}) +
           scale_y_reverse(breaks=1:events$servers) +
           geom_text(aes(x=time/1e3, y=server-ifelse(servers > 5, .6, .4), label=(term %% 10)), vjust=1, size=4) +
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
} else if (exists('multi') && multi) {
  ggsave(multicdf(c('data/submission-normal-WAN',
                    'data/submission-P1-WAN',
                    'data/submission-P2-WAN'),
                  c('f=0', 'f=1', 'f=2'),
                  'Server Failures',
                  legendrows=1),
      filename='data/multi-submission-failures/Rplots.svg',
      width=6,
      height=3)
  ggsave(multicdf(c('data/submission-logsdiff-5-WAN',
                    'data/submission-P1-logsdiff-WAN',
                    'data/submission-P2-logsdiff-WAN'),
                  c('f=0', 'f=1', 'f=2'),
                  'Server Failures',
                  legendrows=1),
      filename='data/multi-submission-failures-logsdiff/Rplots.svg',
      width=6,
      height=3)
  logsdiff_wan <- multicdf_helper(
                      c('data/submission-logsdiff-5-WAN',
                        'data/stalelognobump-logsdiff-5-WAN'),
                      c('submission',
                        'stale-log-no-bump'),
                      'Algorithm')
  logsdiff_ramcloud <- multicdf_helper(
                      c('data/submission-logsdiff-5-RAMCloud',
                        'data/stalelognobump-logsdiff-5-RAMCloud'),
                      c('submission',
                        'stale-log-no-bump'),
                      'Algorithm')
  ggsave(multimulticdf(logsdiff_ramcloud, logsdiff_wan,
                       heights=c(2.15/5,2.15/5,.7/5)),
         filename='data/multi-stalelognobump-logsdiff/Rplots.svg',
         width=6,
         height=6.5)

  logsdiff_wan <- multicdf_helper(
                          c('data/submission-normal-2-WAN',
                            'data/submission-normal-3-WAN',
                            'data/submission-normal-WAN',
                            'data/submission-logsdiff-5-WAN',
                            #'data/submission-logsdiff-5-P2-WAN',
                            'data/submission-logsdiff-9-WAN'),
                          c('2 servers, logs same',
                            '3 servers, logs same',
                            '5 servers, logs same',
                            '5 servers, logs differ',
                            #'5 servers, logs differ, P2',
                            '9 servers, logs differ'),
                          'Server Failures')
  logsdiff_ramcloud <- multicdf_helper(
                          c('data/submission-normal-2-RAMCloud',
                            'data/submission-normal-3-RAMCloud',
                            'data/submission-normal-RAMCloud',
                            'data/submission-logsdiff-5-RAMCloud',
                            'data/submission-logsdiff-9-RAMCloud'),
                          c('2 servers, logs same',
                            '3 servers, logs same',
                            '5 servers, logs same',
                            '5 servers, logs differ',
                            '9 servers, logs differ'),
                          'Server Failures')
  ggsave(multimulticdf(logsdiff_ramcloud, logsdiff_wan,
                       heights=c(2/5,2/5,1/5)),
         filename='data/multi-submission-logsdiff/Rplots.svg',
         width=6,
         height=6.5)
} else {
    cdf(thesis=T)
}
