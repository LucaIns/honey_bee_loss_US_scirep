plot_solAICgg <- function(dat, tit="", 
                          showlegend = T, 
                          ylims = c(NA, NA)) {
  
  xlab = expression(k[p])
  ylab = "rAIC"
  
  p = ggplot(data=dat) +
    geom_line(aes(x=k, y=AIC, colour='rAIC'),
              size=0.5, show.legend = T) +
    geom_point(aes(x=k, y=AIC, colour='rAIC'), 
               show.legend = F, size = 0.75) +
    geom_vline(aes(xintercept=k[AIC==min(AIC)], colour='min(rAIC)'),
               linetype="dashed",  size=1, show.legend = F) +
    labs(x = xlab,
         y = ylab,
         title = tit) +
    ylim(ylims) +
    theme_bw(base_size = 12) +
    theme(text=element_text(size=15))
  
  if (showlegend==1) {
    p + scale_color_manual(values=c('black',NA, 'red'),
                           breaks=c('rAIC', NA, 'min(rAIC)'),)  +
      theme(legend.position=c(.3, .9),
            legend.title = element_blank(),
            legend.box.background=element_rect(),
            legend.box.margin=margin(1,5,5,5),
            legend.key.size = unit(0.6, "cm"),
            legend.text=element_text(size=13),
            legend.key = element_rect(fill = "white")) +
      guides(colour = guide_legend(override.aes = 
                                     list(linetype=c("solid", "dashed"))))
  } else {
    p + scale_color_manual(values=c('black',NA, 'red'),
                           breaks=c('rAIC', NA, 'min(rAIC)'),) + 
      theme(legend.position="none")
  }
}

plot_sol_se_gg <- function(datMat, se = 0, 
                           x = 1:nrow(datMat), 
                           tit="", 
                           ylab = "", 
                           xlab="", 
                           showlegend=T)  {
  
  if(!require(RColorBrewer)){install.packages("RColorBrewer", dependencies = TRUE); library(RColorBrewer)}
  cols = brewer.pal(max(3, length(est_nam)), name="Set1")
  
  if(!require(reshape)){install.packages("reshape", dependencies = TRUE); library(reshape)}
  datMat = melt(datMat, id.vars=NULL)
  se = melt(se, id.vars=NULL)
  names(se) = paste0(names(se), "_se")
  dat = cbind.data.frame(datMat, se)
  dat = cbind.data.frame(dat, x=rep(x, 2))
  
  p = ggplot(data=dat) +
    geom_line(aes(x=x, y=value, group=variable, color=variable),
              show.legend = showlegend, size = 1) +
    geom_point(aes(x=x, y=value, group=variable, color=variable), 
               show.legend = F, size = 2) +
    geom_line(aes(x=x, y=value+value_se, group=variable, color=variable_se),
              linetype="dotted", show.legend = showlegend, size = 1) +
    geom_line(aes(x=x, y=value-value_se, group=variable, color=variable_se),
              linetype="dotted", show.legend = F, size = 1) +
    labs(x = xlab,
         y = ylab,
         title = tit) +
    theme_bw(base_size = 12) +
    theme(text=element_text(size=15))
  
  if (showlegend==1) {
    
    p = p + scale_color_manual(values=c(cols[1], cols[1],
                                        cols[2], cols[2])) +
      theme(legend.position=c(.2, .2),
            legend.title = element_blank(),
            legend.box.background=element_rect(),
            legend.text=element_text(size=13),
            legend.box.margin=margin(1,5,5,5)) +
      guides(colour = guide_legend(override.aes = 
                                     list(linetype=c("solid", "dotted",  
                                                     "solid", "dotted")),
                                   reverse = TRUE))
    p
    
    
  } else {
    p = p + scale_color_manual(values=c(cols[1], cols[1],
                                        cols[2], cols[2])) +
      guides(colour = guide_legend(override.aes = 
                                     list(linetype=c("solid", "dotted",
                                                     "solid", "dotted"))))
    p
  }
  
}
