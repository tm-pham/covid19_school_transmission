# Theme for ggplot 
theme_publication <- function(base_size=14, base_family="helvetica") {
  (theme_foundation(base_size=base_size, base_family=base_family)
   + theme_bw() 
   + theme(axis.line = element_line(colour="black"),
           axis.title = element_text(size=24),
           axis.text=element_text(size=22),
           strip.text = element_text(size=22),
           panel.background = element_rect(colour = NA),
           plot.background = element_rect(colour = NA),
           panel.grid.minor=element_line(color="gray94", linetype = 'solid'),
           panel.grid.major=element_line(color="gray94", linetype = 'solid')
   ))
}
