#  Decision Modelling for Health Economic Evaluation
#  Advanced Course Exercise 4a (Part 1): TEMPLATE FILE
#  Authors: Andrew Briggs, Jack Williams & Nichola Naylor

### !!! set working directory as the folder this is stored in
## added this in to allow for the running of instruction pdf knitting
## whilst reading in data from the same subfolder
## students can ignore if not re-knitting the pdfs, just make sure data files
## are stored in the same file as template/solution files

# require("rstudioapi")  
# setwd(dirname(getActiveDocumentContext()$path)) # Set working directory to source file 


### Loading useful packages
library(ggplot2)
library(reshape2) 

# Reading in the model function from Exercise 3a (Part 1) and ggplot functions
source("A3.3.3a_Presenting_Simulation_Results_Part1_Solutions.R")
source("ggplot_CEA_functions.R")

#### RUNNING THE SIMULATIONS ########
sim.runs <- 1000 ## the number of simulation runs

## creating an empty data.frame for simulation results to fill:
simulation.results <- data.frame("cost.SP0" = rep(as.numeric(NA), sim.runs), ## use the rep() function to create sim.runs rows of values
                                 "qalys.SP0"= rep(as.numeric(NA),sim.runs),
                                 "cost.NP1" = rep(as.numeric(NA),sim.runs),
                                 "qalys.NP1" = rep(as.numeric(NA), sim.runs),
                                 "inc.cost" = rep(as.numeric(NA),sim.runs),
                                 "inc.qalys"=  rep(as.numeric(NA),sim.runs))

## running the simulations and filling the simulation.results data.frame:
for(i in 1:sim.runs){
  simulation.results[i,] <- model.THR(age=60, male=0) ## running the model 1,000 times
}

### Estimating EVPI for individuals ####

WTP <-    ## define a WTP value 

nmb.SP0 <- simulation.results$qalys.SP0 * WTP - simulation.results$cost.SP0  ## estimate the NMB for SP0 
nmb.NP1 <- simulation.results$qalys.NP1 * WTP - simulation.results$cost.NP1  ## estimate the NMB for NP1  
nmb.table <- data.frame(nmb.SP0, nmb.NP1)

av.current <- apply(  )     ## use the apply and mean functions, applying to columns within the nmb.table
  ## This provides the average NMB across all simulations, for each treatment  

max.current <- max(av.current) ## this represnets the maximum NMB for SP0 or NP1 when averaging across 
  #  the NMB for all simulations - this represents 'current knowledge'

max.nmb <- apply(   )        ## use the apply and max functions, applying to rows within the nmb.table
  ## This provides the maximum NMB for each simulations (hence the apply functions moves across rows) 
  ## This selects the NMB for whichever treatment has the highest NMB in each simulation

av.perfect <- mean(max.nmb) ## the average of the vector of maximum NMB's, derived from 
  # having perfect information for each simulation 



EVPI.indiv <-   ## use 2 of the variables defined above, to find the difference between perfect info and current info

#### ESTIMATING POPULATION EVPI #####

# EVPI table
population <-    ## set population number (per year)
years <-     ## set years
evpi.disc <-     ## set discount rate

population.seq <- population * (1/(1+evpi.disc) ^ c(0:(years-1)))
effective.population <-     ## calculate the effective population as a sum of population.seq

pop.EVPI <-     ## calculate the population level EVPI based on variables we've defined in this exercise

## with unknown WTP 

## create a vector from 0 to 50,000 in increments of 100 
WTP.values <- 

est.EVPI.pop <-function(WTP, effective.population,
                        simulation.results) {
  #### FUNCTION: estimating EVPI for effective populations
  ###  INPUTS: WTP - numeric WTP value
  ###          effective.population - numeric effective population value
  ###          results - data.frame object using simulation.runs with 
  ###          qalys.SP0, qalys.NP1, cost.SP0 and cost.NP1 columns
  ###  OUTPUTS: A numeric value for population EVPI
  
  nmb.SP0 <-    ## estimate the NMB for SP0 
  nmb.NP1 <-    ## estimate the NMB for NP1  
  nmb.table <- data.frame(nmb.SP0, nmb.NP1)
  
  av.current <- 
  max.current <- 
  max.nmb <- 
  av.perfect <- 

    
  EVPI.indiv <- 
  pop.EVPI <- 
  
  return(pop.EVPI)
  
}

## create a data frame to capture the WTP values and subsequent population EVPI results
EVPI.results <- data.frame(WTP=    , ## hint: use vector already created
                           EVPI=   )  ## hint: use the rep() function for EVPI for now

## replace NA EVPI values with estimated population EVPI values
for (i in 1:length(WTP.values)) {
  EVPI.results[i,2]<-     ## use est.EVPI.pop()
}

head(EVPI.results)

## ggplot to observe results (the functions created are from from the ggplot functions script)

# Cost-effectiveness plane 
incremental.results <- simulation.results[,c("inc.qalys","inc.cost")]
plot.ce.plane(incremental.results)

# Cost-effectiveness acceptability curve 
plot.ceac(CEAC)

# EVPI, per population
plot.evpi(EVPI.results)
