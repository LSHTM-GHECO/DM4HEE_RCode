## GGplot graphics ## 

# This is a series of functions that use ggplot to create graphs
# ggplot is more flexible than base R plots, and allows users to specify lots of options


if(!require(ggplot2)) install.packages('ggplot2')
library(ggplot2)


##  Exercise 4.7 with sim & 4.8 with sim  ##



## This uses just the incremental costs and outcomes for one intervention 


ce.plane <- function(results, transparency = 0.75){
  
  xlabel = "Incremental QALYs"
  ylabel = "Incremental costs"
  colnames(results) <- c("outcomes", "costs")
  
  plot = ggplot(results) + 
    geom_point(shape = 21, size = 2, colour = "black", fill = NA, alpha = 0.5, aes(x=outcomes, y=costs)) + 
    labs(x = xlabel, text = element_text(size=10)) + 
    labs (y = ylabel, text = element_text(size=10)) + theme_classic() +
    theme(legend.title = element_blank(), axis.title=element_text(face="bold"), 
          axis.title.x = element_text(margin = margin(t = 7, r = 0, b = 3, l = 0)), 
          axis.title.y = element_text(margin = margin(t = 0, r = 7, b = 0, l = 3)), 
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
          legend.key.width=unit(1.8,"line"), text = element_text(size=12),
          plot.margin=unit(c(1.2,0.5,0,1.2),"cm"))
  
  return(plot)
  
}

# ce.plane(simulation.results)



# unfinished 

ce.plane.all <- function(results,  transparency = 0.75){
  
  xlabel = "Incremental QALYs"
  ylabel = "Incremental costs"
  # colnames(results) <- c("outcomes", "costs")
  
  plot = ggplot(results) + 
    geom_point(shape = 21, size = 2, colour = "black", fill = NA, alpha = 0.5, aes(x=outcomes, y=costs, colour = group)) + 
    labs(x = xlabel, text = element_text(size=10)) + 
    labs (y = ylabel, text = element_text(size=10)) + theme_classic() +
    theme(legend.title = element_blank(), axis.title=element_text(face="bold"), 
          axis.title.x = element_text(margin = margin(t = 7, r = 0, b = 3, l = 0)), 
          axis.title.y = element_text(margin = margin(t = 0, r = 7, b = 0, l = 3)), 
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
          legend.key.width=unit(1.8,"line"), text = element_text(size=12),
          plot.margin=unit(c(1.2,0.5,0,1.2),"cm"))
  
  plot
  
}



plot.ceac <- function(results){

  xlabel = "Willingness to pay threshold"
  ylabel = "Probability cost-effective"
  colnames(results) <- c("lambda", "pCE")
  
  plot = ggplot(results) + geom_line(aes(x=lambda, y=pCE), size=1) + 
    labs(x = xlabel, text = element_text(size=10)) + 
    labs(y = ylabel, text = element_text(size=10)) + theme_classic() +
    theme(legend.title = element_blank(), axis.title=element_text(face="bold"), 
          axis.title.x = element_text(margin = margin(t = 7, r = 0, b = 3, l = 0)), 
          axis.title.y = element_text(margin = margin(t = 0, r = 7, b = 0, l = 3)), 
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
          legend.key.width=unit(1.8,"line"), text = element_text(size=12)) + 
    scale_x_continuous(expand = c(0, 0.1)) + 
    scale_y_continuous(limits = c(0,1), breaks=seq(0,1,0.1), expand = c(0, 0))
    
  return(plot)
  
  
}






plot.ceac.all <- function(results){
  
  xlabel = "Willingness to pay threshold"
  ylabel = "Probability cost-effective"
  
  plot = ggplot(results) + geom_line(aes(x=lambda, y=pCE, colour=subgroup), size=1) + 
    labs(x = xlabel, text = element_text(size=10)) + 
    labs(y = ylabel, text = element_text(size=10)) + theme_classic() +
    theme(legend.title = element_blank(), axis.title=element_text(face="bold"), 
          axis.title.x = element_text(margin = margin(t = 7, r = 0, b = 3, l = 0)), 
          axis.title.y = element_text(margin = margin(t = 0, r = 7, b = 0, l = 3)), 
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
          legend.key.width=unit(1.8,"line"), text = element_text(size=12)) + 
    scale_x_continuous(expand = c(0, 0.1)) + 
    scale_y_continuous(limits = c(0,1), breaks=seq(0,1,0.1), expand = c(0, 0))
  
  return(plot)
  
}





plot.evpi <- function(results){
  
  x = 1
  
}




## 

gen.ceac.graph = function(psa, save = FALSE) {
  
  z = ggplot(psa) + geom_line(aes(x=lambda, y=prob.ce), size=0.6) + 
    labs(x = "Willingness to pay (£)", text = element_text(size=4)) + 
    labs (y = "Probability cost-effective", text = element_text(size=4)) + theme_classic() +
    theme(legend.title = element_blank(), axis.title=element_text(face="bold"), 
          axis.title.x = element_text(margin = margin(t = 7, r = 0, b = 3, l = 0)), 
          axis.title.y = element_text(margin = margin(t = 0, r = 7, b = 0, l = 3)), 
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
          legend.key.width=unit(1.8,"line"), text = element_text(size=7),
          plot.margin=unit(c(1.2,0.5,0,1.2),"cm")) + 
    scale_x_continuous(labels = scales::comma, breaks = c(seq(0,100000,5000)), limits = c(0,40000), expand = c(0, 0.1)) + 
    scale_y_continuous(limits = c(0,1), breaks=seq(0,1,0.1), expand = c(0, 0)) + 
    geom_vline(xintercept = 20000, linetype="dotted", size=0.25)
  
  
  if(save == TRUE) ggsave(paste("figures\\CEAC",Sys.Date(),".png"), z, width=107, height=70, dpi=300, units='mm')
  
  
  return(z)
  
}

gen.evpi.graph = function(evpi, save = FALSE) {
  
  z = ggplot(evpi) + geom_line(aes(x=lambda, y=evpi.m), size=0.6) + 
    labs(x = "Willingness to pay (£)", text = element_text(size=4)) + 
    labs (y = "EVPI (£)", text = element_text(size=4)) + theme_classic() +
    theme(legend.title = element_blank(), axis.title=element_text(face="bold"), 
          axis.title.x = element_text(margin = margin(t = 7, r = 0, b = 3, l = 0)), 
          axis.title.y = element_text(margin = margin(t = 0, r = 7, b = 0, l = 3)), 
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
          legend.key.width=unit(1.8,"line"), text = element_text(size=7),
          plot.margin=unit(c(1.2,0.5,0,1.2),"cm")) + 
    scale_x_continuous(labels = scales::comma, breaks = c(seq(0,100000,5000)), limits = c(0,40000), expand = c(0, 0.1)) + 
    scale_y_continuous(expand = c(0, 0)) + 
    geom_vline(xintercept = 20000, linetype="dotted", size=0.25)
  
  if(save == TRUE) ggsave(paste("figures\\EVPI",Sys.Date(),".png"), z, width=107, height=70, dpi=300, units='mm')
  
  return(z)
  
}


