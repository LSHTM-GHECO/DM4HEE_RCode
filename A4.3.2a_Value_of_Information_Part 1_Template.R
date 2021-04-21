#  Decision Modelling for Health Economic Evaluation
#  Advanced Course Exercise 4a (Part 1): TEMPLATE FILE
#  Authors: Andrew Briggs, Jack Williams & Nichola Naylor

### Loading useful packages
library(data.table)
library(tidyr)
library(dplyr)
library(ggplot2)
library(reshape2) 

# Reading in the model function from Exercise 3a (Part 1)
source("A3.3.3a_Presenting_Simulation_Results_Part1_Solutions.R")

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

WTP <-    ## define a WTP value of 100,000

## we turn simulation.results into a data.table 
simulation.results <- as.data.table(simulation.results)

simulation.results[ , nmbSP0 :=     ] ## estimate the NMB for SP0 
simulation.results[ , nmbNP1 :=     ] ## estimate the NMB for NP1  

sim.means <-      ## use the apply and mean functions, applying to columns 7 & 8 of simulation results
max.nmb <-        ## use the apply and mean functions, applying to rows for columns 7 & 8 of simulation results
av.perfect <- mean(max.nmb) ## the average maximum NMB for each run
max.sim.mean <- max(sim.means) ## maximum of the simulation mean values

EVPI.indiv <-   ## use 2 of the variables defined above


#### ESTIMATING POPULATION EVPI #####

# EVPI table
population <-    ## set population number 
years <-     ## set years
evpi.disc <-     ## set discount rate

population.seq <- sum(population * (1/(1+evpi.disc) ^ c(0:(years-1))))
effective.population <-     ## calculate the effective population as a sum of population.seq

pop.EVPI <-     ## calculate the population level EVPI based on variables we've defined in this exercise

## with unknown WTP 

## create a vector from 0 to 100,000 in increments of 100 
WTP.values <- 

est.EVPI.pop <-function(WTP, effective.population,
                        simulation.results) {
  #### FUNCTION: estimating EVPI for effective populations
  ###  INPUTS: WTP - numeric WTP value
  ###          effective.population - numeric effective population value
  ###          results - data.table object from simulation.runs with 
  ###          qalys.SP0 and cost.SP0 columns
  ###  OUTPUTS: A numeric value for population EVPI
  temp <- as.data.table(simulation.results)
  
  temp[ , nmbSP0 :=   ]
  temp[ , nmbNP1 :=   ]
  
  sim.means <- 
  max.nmb <- 
  av.perfect <- 
  
  EVPI.indiv <- 
  pop.EVPI <- 
  
  return(pop.EVPI)
  
}

## create a data frame to capture the WTP values and subsequent population EVPI results
EVPI.results <- data.frame(WTP=    , 
                           EVPI=   )  ## hint: use the rep() function for EVPI for now

## replace NA EVPI values with estimated population EVPI values
for (i in 1:length(WTP.values)) {
  EVPI.results[i,1] <- WTP.values[i]
  EVPI.results[i,2]<-     ## use est.EVPI.pop()
}

head(EVPI)

### NEED TO UPDATE THE GRAPHS TO WORK?
# ce.plane(incremental.results)
# 
# plot.ceac(CEAC)
# 
# plot.evpi(EVPI.patient)
# 
# plot.evpi(EVPI.population)
