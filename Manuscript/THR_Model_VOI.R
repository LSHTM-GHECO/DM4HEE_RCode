# Loading in data and model
source("Manuscript/THR_Model.R")

## View the PSA results 
simulation.results


#### SETTING VALUE OF INFORMATION POPULATION PARAMETERS ####
population <- 40000 
years <- 10
evpi.disc <- 0.06
population.seq <- population * (1/(1+evpi.disc) ^ c(0:(years-1)))
effective.population <- sum(population.seq)



## EXPECTED VALUE OF PERFECT INFORMATION (EVPI)

## Create a vector of willingness to pay values
WTP.values <- seq(from = 0, to = 50000, by = 100)

# Create a function to estimate EVPI (at a population level)
est.EVPI.pop <-function(WTP, effective.population, simulation.results) {
  ###  INPUTS: WTP - numeric WTP value
  ###          effective.population - numeric effective population value
  ###          results - data.frame object from simulation.runs with 
  ###          qalys.SP0 and cost.SP0 columns & equivalents for NP1
  ###  OUTPUTS: A numeric value for population EVPI
  
  nmb.SP0 <- simulation.results$qalys.SP0 * WTP - simulation.results$cost.SP0
  nmb.NP1 <- simulation.results$qalys.NP1 * WTP - simulation.results$cost.NP1
  nmb.table <- data.frame(nmb.SP0, nmb.NP1)
  
  av.current <- apply(nmb.table, 2, mean)
  max.current <- max(av.current)
  max.nmb <- apply(nmb.table, 1, max) 
  av.perfect <- mean(max.nmb)
  
  EVPI.indiv <- av.perfect - max.current
  pop.EVPI <- effective.population * EVPI.indiv
  
  return(pop.EVPI)
  
}

## create a data frame to capture the WTP values and subsequent population EVPI results 

EVPI.results <- data.frame(WTP=WTP.values, EVPI=NA)

for (i in 1:length(WTP.values)) {
  EVPI.results[i,2] <- est.EVPI.pop(WTP.values[i], effective.population, simulation.results)
}

## Show the highest EVPI value
EVPI.results[EVPI.results$EVPI == max(EVPI.results$EVPI),] 

## Plot results EVPI, per population
evpi.plot <- ggplot(EVPI.results) + geom_line(aes(x=WTP, y=EVPI), size=1) + 
  labs(x = "Willingness to pay threshold", text = element_text(size=10)) + 
  labs(y = "Expected Value of Perfect Information", text = element_text(size=10)) + theme_classic() +
  theme(legend.title = element_blank(), axis.title=element_text(face="bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        legend.key.width=unit(1.8,"line"), text = element_text(size=12),
        axis.title.x = element_text(margin = margin(t = 7, r = 0, b = 3, l = 0)), 
        axis.title.y = element_text(margin = margin(t = 0, r = 7, b = 0, l = 3)), 
        plot.margin=unit(c(0.5,0.5,0,0.5),"cm")) + 
  scale_x_continuous(labels = scales::comma, expand = c(0, 0.1)) + 
  scale_y_continuous(labels = scales::comma, expand = c(0, 0))

evpi.plot


#### EXPECTED VALUE OF PARTIAL PERFECT INFORMATION (EVPPI) ANALYSIS #### 

## Enter inner and outer loop numbers - note these must be higher than sim.runs 
inner.loops <- 100
outer.loops <- 100


# Create a matrix to store the inner loop results
inner.results <- matrix(0, inner.loops, 4)
colnames(inner.results) <- c("Cost SP0", "QALY SP0", "Cost NP1", "QALY NP1", "Incr Cost", "Incr QALY")

evppi.results.SP0 <- matrix(0, nrow = outer.loops, ncol = length(WTP.values))
colnames(evppi.results.SP0) <- as.character(WTP.values)
evppi.results.NP1 <- evppi.results.SP0 



# creating a function to estimate NMB across WTP values:

nmb.function <- function(WTP, results){
  #### FUNCTION: Estimate NMB across all the WTP values 
  ###  INPUTS: a WTP value vector and a results data.frame
  ###         "Cost SP0" "QALY SP0" "Cost NP1" "QALY NP1"

  nmb.table <- matrix(WTP, ncol = length(WTP), nrow = dim(results)[1],  byrow = TRUE) 
  
  SP0 <- ((results[,"QALY SP0"] * nmb.table) - results[,"Cost SP0"])  
  NP1 <- ((results[,"QALY NP1"] * nmb.table) - results[,"Cost NP1"])
  
  nmb.p <- apply(SP0, 2, mean)
  nmb.t <- apply(NP1, 2, mean) 
    
  ###  OUTPUTS: a list of the NMB under SP0 (nmb.p) and NP1 (nmb.t)
  return(list(nmb.t, nmb.p))
  
}


## Function to estimate EVPPI across WTP values, using the stored EVPPI results
gen.evppi.results <- function(evppi.results1 = evppi.results.SP0, evppi.results2 = evppi.results.NP1, WTP = WTP.values){
  ### FUNCTION: Function to estimate EVPPI across WTP values, using the stored EVPPI results
  ### INPUTS: matrix arrays of evppi.results.SP0 and evppi.results.NP1, and
  ###         a vector of WTP values
  
  ## calculate the mean NMB for current and new treatments, at each WTP 
  current.info1 <- apply(evppi.results1, 2, mean)
  current.info2 <- apply(evppi.results2, 2, mean)
  
  ## calculate the max NMB (overall) from either current or new treatments, at each WTP
  current.info <- pmax(current.info1, current.info2)
  
  # Create an array 
  evppi.array <- array(0, dim = c(outer.loops, length(WTP), 2))
  evppi.array[,,1] <- evppi.results1
  evppi.array[,,2] <- evppi.results2
  
  # calculate the max NMB for each treatment, at each WTP, for each simulation 
  # This is so that the best treatment can be select for each simulation
  perf.info.sims <- apply(evppi.array, c(1,2), max)
  perf.info <- apply(perf.info.sims, 2, mean)
  
  # Work out the difference between perfect information (for each simulation) vs. current information, at each WTP
  evppi.results <- c(perf.info - current.info)
  
  ### OUTPUT: a data.frame of different WTP and corresponding evppi values     
  evppi <- data.frame(WTP, evppi.results)
  return(evppi)
  
}



# EVPPI Sampling 

## Now the EVPPI loops will be run - each selected different values for inner and outer loops


#### EVPPI loops - NP1 parameter  ####

pb = txtProgressBar(min = 0, max = outer.loops, initial = 0, style = 3)

for(a in 1:outer.loops){
  
  for(b in 1:inner.loops){
    
    # The 'partial' parameter will be included in the outer loop - so we can select that using 'a' in the outer loop
    # The parameters included in the inner loop remain are selected using 'b'
    
    inner.results[b,] <-  model.THR(RR.vec[a], omr.df[b,],  tp.rrr.vec[b], 
                                        survival.df[b,],c.revision.vec[b], 
                                        state.utilities.df[b,], mortality.vec = mortality.vec)[1:4]
    
  }
  
  #after each inner loop PSA, calculate the mean NMB for each treatment comparator and store the results
  nmb <- nmb.function(WTP.values, inner.results)
  
  evppi.results.SP0[a,] <- nmb[[1]]
  evppi.results.NP1[a,] <- nmb[[2]]
  setTxtProgressBar(pb,a)
  
}

NP1.evppi <- gen.evppi.results()



#### EVPPI loops - OMR transition parameters  ####

pb = txtProgressBar(min = 0, max = outer.loops, initial = 0, style = 3)

for(a in 1:outer.loops){
  
  for(b in 1:inner.loops){
    inner.results[b,] <-  model.THR(RR.vec[b], omr.df[a,],  tp.rrr.vec[b], 
                                        survival.df[b,],c.revision.vec[b], 
                                        state.utilities.df[b,], mortality.vec = mortality.vec)[1:4]
  }
  
  #after each inner loop PSA, calculate the mean NMB for each tx and store the results
  nmb <- nmb.function(WTP.values, inner.results)
  evppi.results.SP0[a,] <- nmb[[1]]
  evppi.results.NP1[a,] <- nmb[[2]]
  setTxtProgressBar(pb,a)
}

omr.evppi <- gen.evppi.results()

#### EVPPI loops - Re-revision risk parameter ####

pb = txtProgressBar(min = 0, max = outer.loops, initial = 0, style = 3)

for(a in 1:outer.loops){
  
  for(b in 1:inner.loops){
    
    inner.results[b,] <-  model.THR(RR.vec[b], omr.df[b,],
                                        tp.rrr.vec[a], 
                                        survival.df[b,],c.revision.vec[b], 
                                        state.utilities.df[b,], mortality.vec = mortality.vec)[1:4]
    
  }
  
  #after each inner loop PSA, calculate the mean NMB for each tx and store the results
  nmb <- nmb.function(WTP.values, inner.results)
  evppi.results.SP0[a,] <- nmb[[1]]
  evppi.results.NP1[a,] <- nmb[[2]]
  setTxtProgressBar(pb,a)
}

rrr.evppi <- gen.evppi.results()

#### EVPPI loops - Survival parameters  ####

pb = txtProgressBar(min = 0, max = outer.loops, initial = 0, style = 3)

for(a in 1:outer.loops){
  
  for(b in 1:inner.loops){
    
    inner.results[b,] <-  model.THR(RR.vec[b], omr.df[b,],
                                        tp.rrr.vec[b], 
                                        survival.df[a,],c.revision.vec[b], 
                                        state.utilities.df[b,], mortality.vec = mortality.vec)[1:4]
  }
  
  #after each inner loop PSA, calculate the mean NMB for each tx and store the results
  nmb <- nmb.function(WTP.values, inner.results)
  evppi.results.SP0[a,] <- nmb[[1]]
  evppi.results.NP1[a,] <- nmb[[2]]
  setTxtProgressBar(pb,a)
}

surv.evppi <- gen.evppi.results()

#### EVPPI loops - Cost revision  ####

pb = txtProgressBar(min = 0, max = outer.loops, initial = 0, style = 3)

for(a in 1:outer.loops){
  
  for(b in 1:inner.loops){
    inner.results[b,] <-  model.THR(RR.vec[b], omr.df[b,],tp.rrr.vec[b], 
                                        survival.df[b,],
                                        c.revision.vec[a], 
                                        state.utilities.df[b,], mortality.vec = mortality.vec)[1:4]
  }
  
  #after each inner loop PSA, calculate the mean NMB for each tx and store the results
  nmb <- nmb.function(WTP.values, inner.results)
  evppi.results.SP0[a,] <- nmb[[1]]
  evppi.results.NP1[a,] <- nmb[[2]]
  setTxtProgressBar(pb,a)
}

cRevision.evppi <- gen.evppi.results()

#### EVPPI loops - Utilities  ####

pb = txtProgressBar(min = 0, max = outer.loops, initial = 0, style = 3)

for(a in 1:outer.loops){
  
  for(b in 1:inner.loops){
    inner.results[b,] <-  model.THR(RR.vec[b], omr.df[b,],tp.rrr.vec[b], 
                                        survival.df[b,],c.revision.vec[b], 
                                        state.utilities.df[a,], mortality.vec = mortality.vec)[1:4] 
  }
  
  #after each inner loop PSA, calculate the mean NMB for each tx and store the results
  nmb <- nmb.function(WTP.values, inner.results)
  evppi.results.SP0[a,] <- nmb[[1]]
  evppi.results.NP1[a,] <- nmb[[2]]
  setTxtProgressBar(pb,a)
}

utilities.evppi <- gen.evppi.results()

#### Analysis of EVPPI results #### 

# Create a data frame with all EVPPI results

evppi.wide.patient <- data.frame(NP1.evppi,
                                 surv.evppi[,2],
                                 utilities.evppi[,2],
                                 cRevision.evppi[,2],
                                 omr.evppi[,2],
                                 rrr.evppi[,2])

colnames(evppi.wide.patient) <- c("WTP", "NP1 Relative risk", "Survival parameters", "Utilities", "Revision cost", "Operative mortality ratios", "Re-revision risk")

# The wide format is because there is a column for each parameter set included in the EVPPI 
head(evppi.wide.patient)

# Transform from per patient EVPPI to population EVPPI 
evppi.wide.pop <- evppi.wide.patient * effective.population
evppi.wide.pop[,1] <- evppi.wide.patient[,1]

# For plotting (in ggplot) the wide format data needs to be converted to long format 
evppi.long.pop <- reshape2::melt(evppi.wide.pop, id.vars = c("WTP"))

# Now the parameter is a variable of it's own (i.e. the EVPPI results are stacked on top of each other)
head(evppi.long.pop, 10)


## Plot
plot.evppi <- ggplot(evppi.long.pop) + geom_line(aes(x=WTP, y=value, colour = variable), size=0.75) + 
                labs(x = "Willingness to pay threshold", text = element_text(size=10)) + 
                labs(y = "EVPPI", text = element_text(size=10)) + theme_classic() +
                theme(legend.title = element_blank(), axis.title=element_text(face="bold"), 
                      axis.title.x = element_text(margin = margin(t = 7, r = 0, b = 3, l = 0)), 
                      axis.title.y = element_text(margin = margin(t = 0, r = 7, b = 0, l = 3)), 
                      panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                      legend.key.width=unit(1.8,"line"), text = element_text(size=12),
                      plot.margin=unit(c(0.5,0,0,0.5),"cm")) + 
                scale_x_continuous(labels = scales::comma, limits = c(0, 10000), expand = c(0, 0.1)) + 
                scale_y_continuous(labels = scales::comma, expand = c(0, 0))

plot.evppi
# Note you will get a warning here that the plot does not show all the data in the data.frame
# This is because the plot only shows WTP from 0 to 15000, rather than the 50000 maximum in the data.frame


# If you want to retrieve the EVPPI at a specific point
subset(evppi.long.pop, WTP==2200)

# and we can plot this:
sub.evppi <- subset(evppi.long.pop, WTP==2200)

plot.sub.evppi <- ggplot(sub.evppi, aes(x=variable, y=value)) +
                    geom_bar(stat="identity", fill="steelblue")+
                    labs(x = "Parameter Group", text = element_text(size=4)) + 
                    labs(y = "EVPPI", text = element_text(size=4)) + theme_classic() +
                    theme(legend.title = element_blank(), axis.title=element_text(face="bold"), 
                          axis.title.x = element_text(margin = margin(t = 7, r = 0, b = 3, l = 0)), 
                          axis.title.y = element_text(margin = margin(t = 0, r = 7, b = 0, l = 3)),
                          axis.text.x=element_text(angle=45,hjust=1), 
                          panel.grid.major = element_line(), panel.grid.minor = element_line(), 
                          legend.key.width=unit(1.8,"line"), text = element_text(size=12)) + 
                    scale_y_continuous(labels = scales::comma, expand = c(0, 0))

plot.sub.evppi
