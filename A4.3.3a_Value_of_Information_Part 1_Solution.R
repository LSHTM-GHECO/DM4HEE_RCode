#  Decision Modelling for Health Economic Evaluation
#  Advanced Course Exercise 4a (Part 1): SOLUTION FILE
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

WTP <- 100000

# simulation.results <- as.data.table(simulation.results)
# simulation.results[ , nmbSP0 := qalys.SP0*WTP - cost.SP0]
# simulation.results[ , nmbNP1 := qalys.NP1*WTP - cost.NP1]

nmb.SP0 <- simulation.results$qalys.SP0 * WTP - simulation.results$cost.SP0
nmb.NP1 <- simulation.results$qalys.NP1 * WTP - simulation.results$cost.NP1
nmb.table <- data.frame(nmb.SP0, nmb.NP1)

av.current <- apply(nmb.table, 2, mean)
max.nmb <- apply(nmb.table, 1, max) 
av.perfect <- mean(max.nmb)
max.current <-  max(av.current)

EVPI.indiv <- av.perfect - max.current


#### ESTIMATING POPULATION EVPI #####

# EVPI table
population <- 40000 
years <- 10
evpi.disc <- 0.06

population.seq <- sum(population * (1/(1+evpi.disc) ^ c(0:(years-1))))
effective.population <- sum(population.seq)

pop.EVPI <- effective.population*EVPI.indiv

## with unknown WTP 

## create a vector from 0 to 50,000 in increments of 100 
WTP.values <- seq(from = 0, to = 50000, by = 100)

est.EVPI.pop <-function(WTP, effective.population,
                        simulation.results) {
  #### FUNCTION: estimating EVPI for effective populations
  ###  INPUTS: WTP - numeric WTP value
  ###          effective.population - numeric effective population value
  ###          results - data.table object from simulation.runs with 
  ###          qalys.SP0 and cost.SP0 columns
  ###  OUTPUTS: A numeric value for population EVPI

  nmb.SP0 <- simulation.results$qalys.SP0 * WTP - simulation.results$cost.SP0
  nmb.NP1 <- simulation.results$qalys.NP1 * WTP - simulation.results$cost.NP1
  nmb.table <- data.frame(nmb.SP0, nmb.NP1)

  av.current <- apply(nmb.table, 2, mean)
  max.nmb <- apply(nmb.table, 1, max) 
  av.perfect <- mean(max.nmb)
  
  EVPI.indiv <- av.perfect - max(av.current)
  pop.EVPI <- effective.population * EVPI.indiv

  return(pop.EVPI)
  
}

## create a data frame to capture the WTP values and subsequent population EVPI results 

EVPI.results <- data.frame(WTP=WTP.values, 
                           EVPI=rep(as.numeric(NA),length(WTP.values)))


for (i in 1:length(WTP.values)) {
  EVPI.results[i,1] <- WTP.values[i]
  EVPI.results[i,2]<-  est.EVPI.pop(WTP.values[i], 
                                    effective.population,
                                    simulation.results)
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
