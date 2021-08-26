#  Decision Modelling for Health Economic Evaluation
#  Plotting functions using ggplot
#  Authors: Jack Williams & Nichola Naylor

# This is a series of functions that use ggplot to create graphs
# ggplot is more flexible than base R plots, and allows users to specify many of options

# Install package and load library
if(!require(ggplot2)) install.packages('ggplot2')
library(ggplot2)


plot.ce.plane <- function(results){
  ### FUNCTION: PLOTTING THE COST-EFFECTIVENESS PLANE (for one comparator, using incremetnal costs and outcomes)
  ### INPUTS: a results data frame that has the columns "inc.qalys" and "inc.cost"
  ### Need ggplot called
  ### OUTPUTS: cost-effectiveness plane plot
  
  xlabel = "Incremental QALYs"
  ylabel = "Incremental costs"
  
  plot = ggplot(results) + 
    geom_point(shape = 21, size = 2, colour = "black", fill = NA, alpha = 0.5, aes(x=inc.qalys, y=inc.cost)) + 
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


plot.ceac <- function(results){
  ### FUNCTION: PLOTTING THE COST-EFFECTIVENESS ACCEPTABILITY CURVE FOR ONE COMPARATOR
  ### INPUTS: a results data frame that has the columns "WTP"& "pCE"
  ### Need ggplot called
  ### OUTPUTS: cost-acceptability curve plot
  
  xlabel = "Willingness to pay threshold"
  ylabel = "Probability cost-effective"
  
  plot = ggplot(results) + geom_line(aes(x=WTP, y=pCE), size=1) + 
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
  ## FUNCTION: Cost-effectiveness acceptability curve (CEAC) for multiple comparators
  ## INPUTS: a results data frame that has the columns "WTP", "group"& "pCE"
  ## OUTPUT: CEAC curve where the color of the CEAC differs for each comparator
  
  xlabel = "Willingness to pay threshold"
  ylabel = "Probability cost-effective"
  
  plot = ggplot(results) + geom_line(aes(x=WTP, y=pCE, color=group), size=1) + 
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


plot.ce.plane.all <- function(results){
  ### FUNCTION: PLOTTING THE COST-EFFECTIVENESS PLANE  (for multiple comparators)
  ### INPUTS: a results data frame that has the columns "cost", "qaly" and "comparator" columns defined
  ### Need ggplot called
  ### OUTPUTS: ggplot cost-effectiveness plane plot where the colour of the circles relates to the comparator
  
  ## results must have "qaly", "cost" 
  xlabel = "QALYs"
  ylabel = "Costs"
  
  plot = ggplot(results) + 
    geom_point(shape = 21, size = 2, aes(x=qaly, y=cost, colour = comparator)) + 
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

plot.evpi <- function(results){
  ### FUNCTION: Expected Value of Perfect Information (EVPI)
  ### INPUTS: a data frame with the columns "WTP" and "EVPI" (numerics)
  ### OUTPUTS: a plot of EVPI vs WTP
  
  xlabel = "Willingness to pay threshold"
  ylabel = "Expected Value of Perfect Information"
  
  plot = ggplot(results) + geom_line(aes(x=WTP, y=EVPI), size=1) + 
    labs(x = xlabel, text = element_text(size=10)) + 
    labs(y = ylabel, text = element_text(size=10)) + theme_classic() +
    theme(legend.title = element_blank(), axis.title=element_text(face="bold"), 
          axis.title.x = element_text(margin = margin(t = 7, r = 0, b = 3, l = 0)), 
          axis.title.y = element_text(margin = margin(t = 0, r = 7, b = 0, l = 3)), 
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
          legend.key.width=unit(1.8,"line"), text = element_text(size=12),
          plot.margin=unit(c(0.5,0.5,0,0.5),"cm")) + 
    scale_x_continuous(labels = scales::comma, expand = c(0, 0.1)) + 
    scale_y_continuous(labels = scales::comma, limits = c(0, max(results$EVPI)*1.1), expand = c(0, 0))
  
  return(plot)
  
}


plot.evppi <- function(results, xlimit = 15000){
  ## FUNCTION: Expected Value of Partial Perfect Information (EVPPI)
  ## This function allows for a limit on the X-axis, to help show the graph with adjusted axes
  ## INPUTS: results table equivalent to evppi.long.pop (see A3.3.b solutions)
  ##         with "WTP", "variable" and "value" columns
  ## OUTPUTS: a plot of EVPPI with specifiable x-axis value limit
  
  xlabel = "Willingness to pay threshold"
  ylabel = "Expected Value of Perfect Partial Information"
  
  plot = ggplot(results) + geom_line(aes(x=WTP, y=value, colour = variable), size=0.75) + 
    labs(x = xlabel, text = element_text(size=10)) + 
    labs(y = ylabel, text = element_text(size=10)) + theme_classic() +
    theme(legend.title = element_blank(), axis.title=element_text(face="bold"), 
          axis.title.x = element_text(margin = margin(t = 7, r = 0, b = 3, l = 0)), 
          axis.title.y = element_text(margin = margin(t = 0, r = 7, b = 0, l = 3)), 
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
          legend.key.width=unit(1.8,"line"), text = element_text(size=12),
          plot.margin=unit(c(0.5,0,0,0.5),"cm")) + 
    scale_x_continuous(labels = scales::comma, limits = c(0, xlimit), expand = c(0, 0.1)) + 
    scale_y_continuous(labels = scales::comma, expand = c(0, 0))
  
  return(plot)
  
}

plot.sub.evppi <-function(results){
  ## FUNCTION: Expected Value of Partial Perfect Information (EVPPI) for a particular WTP threshold value
  ## INPUTS: results table equivalent to evppi.long.pop (see A3.3.b solutions) for a set WTP subgroup
  ##         with "WTP", "variable" and "value" columns
  ## OUTPUTS: a plot of parameter group EVPPI for that WTP

xlabel = "Parameter Group"
ylabel = "EVPPI"

plot = ggplot(data=results, aes(x=variable, y=value)) +
  geom_bar(stat="identity", fill="steelblue")+
  labs(x = xlabel, text = element_text(size=4)) + 
  labs(y = ylabel, text = element_text(size=4)) + theme_classic() +
  theme(legend.title = element_blank(), axis.title=element_text(face="bold"), 
        axis.title.x = element_text(margin = margin(t = 7, r = 0, b = 3, l = 0)), 
        axis.title.y = element_text(margin = margin(t = 0, r = 7, b = 0, l = 3)),
        axis.text.x=element_text(angle=45,hjust=1), 
        panel.grid.major = element_line(), panel.grid.minor = element_line(), 
        legend.key.width=unit(1.8,"line"), text = element_text(size=12)) + 
  scale_y_continuous(labels = scales::comma, expand = c(0, 0))

return(plot)
}