require("ggplot2")
require("gridExtra")
require("scales")

gtheme <- theme_bw(base_size = 12, base_family = "") +
          theme(plot.margin=unit(c(.2,0,0,0), 'cm'),
                legend.margin=unit(0, 'cm'),
                axis.title=element_text(size = rel(.8)),
                plot.title=element_text(size = rel(.8)))

plotwd <- function() {

    meta = read.csv('meta.csv')
    events = read.csv('events.csv')
    events$state = factor(events$state,
                          levels=c('F', 'C', 'L'))

    g <- {}

    g$timeline <- ggplot(events) + gtheme +
           geom_point(aes(x=time/1e3, y=server, color=state)) +
           geom_text(aes(x=time/1e3, y=server+.1, label=term), vjust=0) +
           xlab('Time (ms)') +
           ylab('Server')
    ggsave(plot=g$timeline,
           filename='timeline.svg',
           width=7, height=3.5)

}

if (exists('repl') && repl) {
    home <- getwd()
    while (TRUE) {
        dir <- readline('dir: ')
        if (dir != '') {
            setwd(dir)
            plotwd()
            setwd(home)
            write('--DONE--\n', '')
        }
    }
} else {
    plotwd()
}
