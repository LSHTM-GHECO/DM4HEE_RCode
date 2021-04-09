## ggplot graphics ## 

# This is a series of functions that use ggplot to create graphs
# ggplot is more flexible than base R plots, and allows users to specify many of options



# Install package and load library
if(!require(ggplot2)) install.packages('ggplot2')
library(ggplot2)




## Cost-effectiveness plane (for one comparator, using incremetnal costs and outcomes)

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





## Cost-effectiveness plane (for multiple comparators) - this is essentially the same, except that 
# the colour of the circles relates to the comparator

ce.plane.all <- function(results,  transparency = 0.75){
  
  xlabel = "QALYs"
  ylabel = "Costs"

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
  
  return(plot)
  
}


# Cost-effectiveness acceptability curve (CEAC) for one comparator

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



# Cost-effectiveness acceptability curve (CEAC) for multiple comparators
# similar to the cost-effectiveness plane, the only difference in the code is that 
# the color of the CEAC differs for each comparator


plot.ceac.all <- function(results){
  
  xlabel = "Willingness to pay threshold"
  ylabel = "Probability cost-effective"
  colnames(results) <- c("lambda", "group", "pCE")
  
  plot = ggplot(results) + geom_line(aes(x=lambda, y=pCE, color=group), size=1) + 
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



# Expected Value of Perfect Information (EVPI)


plot.evpi <- function(results){
  
  xlabel = "Willingness to pay threshold"
  ylabel = "Expected Value of Perfect Information"
  colnames(results) <- c("lambda", "EVPI")
  
  plot = ggplot(results) + geom_line(aes(x=lambda, y=EVPI), size=1) + 
    labs(x = xlabel, text = element_text(size=10)) + 
    labs(y = ylabel, text = element_text(size=10)) + theme_classic() +
    theme(legend.title = element_blank(), axis.title=element_text(face="bold"), 
          axis.title.x = element_text(margin = margin(t = 7, r = 0, b = 3, l = 0)), 
          axis.title.y = element_text(margin = margin(t = 0, r = 7, b = 0, l = 3)), 
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
          legend.key.width=unit(1.8,"line"), text = element_text(size=12)) + 
    scale_x_continuous(labels = scales::comma, expand = c(0, 0.1)) + 
    scale_y_continuous(labels = scales::comma, expand = c(0, 0))
  
  return(plot)
  
}


# Expected Value of Partial Perfect Information (EVPPI)
# This function allows for a limit on the X-axis, to help show the graph with adjusted axes

plot.evppi <- function(results, xlimit = 20000){
  
  xlabel = "Willingness to pay threshold"
  ylabel = "Expected Value of Perfect Partial Information"
  
  plot = ggplot(results) + geom_line(aes(x=lambda, y=value, colour = variable), size=1) + 
    labs(x = xlabel, text = element_text(size=10)) + 
    labs(y = ylabel, text = element_text(size=10)) + theme_classic() +
    theme(legend.title = element_blank(), axis.title=element_text(face="bold"), 
          axis.title.x = element_text(margin = margin(t = 7, r = 0, b = 3, l = 0)), 
          axis.title.y = element_text(margin = margin(t = 0, r = 7, b = 0, l = 3)), 
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
          legend.key.width=unit(1.8,"line"), text = element_text(size=12)) + 
    scale_x_continuous(labels = scales::comma, limits = c(0, xlimit), expand = c(0, 0.1)) + 
    scale_y_continuous(labels = scales::comma, expand = c(0, 0))
  
  return(plot)
  
}




