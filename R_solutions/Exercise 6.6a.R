#  DM4HEE 
#  Exercise 6.6 - Introducing a third prosthesis into the THR model
#  Author:  Jack Williams & Nichola Naylor
#  Date created: 18 March 2021
#  Date last edit: 25 March 2021


# Load packages 
library(data.table)


# Run pre-set code for ggplot graphics 
source('graphs/ggplot functions.R')


# Number of model simulations (for probabilistic analyses) - EVPPI loops can be changed below
sim.runs <- 10000

#  Demographics

age <- 60
male <- 0


#  Read in the life table and covariance table
life.table <- read.csv("inputs/life-table.csv")
life.table <- data.table(life.table)
cov.55 <- read.csv("inputs/cov55.csv",row.names=1,header=TRUE) 


# Define fixed parameters outside of the model to avoid repition

cDR<-0.06
oDR<-0.015

#  Seed the starting states of the model
seed<-c(1,0,0,0,0)

#  Set the total number of cycles to run and name the states
cycles<-60


state.names<-c("P-THR","successP-THR","R-THR","successR-THR","Death")
n.states<-length(state.names)


# Discounting matrices

O.discount.factor <- matrix(1/(1+oDR) ^ c(1:cycles), nrow = 1, ncol = cycles)
C.discount.factor <- matrix(1/(1+cDR) ^ c(1:cycles), nrow = 1, ncol = cycles)


#  Defining parameters


#  Transition probabilities
  
omrPTHR <- rbeta(sim.runs,2,98)
omrRTHR <- rbeta(sim.runs,2,98)
rrr <- rbeta(sim.runs,4,96)
  
# create transitions data.frame to pass into the model function
transitions <- data.frame(omrPTHR = omrPTHR, omrRTHR = omrRTHR, rrr = rrr)
  
#  Revision rates
  
mn.cons <- -5.490935
mn.ageC <- -0.0367022
mn.maleC <- 0.768536
mn.NP1 <- -1.344474
mn.lngamma <- 0.3740968
mn <- c(mn.lngamma, mn.cons,mn.ageC,mn.maleC,mn.NP1)
  
cholm <- t(chol(t(cov.55))) 
  
z.mat <- matrix(rnorm(sim.runs*5, 0, 1), nrow = sim.runs, ncol = 5)
  
revision.LP <- matrix(NA, nrow = sim.runs, ncol = 4)
colnames(revision.LP) <- c("standard", "NP1", "lngamma")

for(i in 1:sim.runs){
    
  x <- mn + cholm %*% z.mat[i,]
  
  lngamma <- x[1,1]
  cons <- x[2,1]
  ageC <- x[3,1]
  maleC <- x[4,1]
  NP1 <- x[5,1]
  
  revision.LP[i,1] <- cons+age*ageC+male*maleC
  revision.LP[i,2] <- np1.LP<-cons+age*ageC+male*maleC+NP1
  revision.LP[i,3] <- lngamma
  revision.LP[i,4] <- NP1 
  # the NP1 parameter values are not needed for the PSA, but are stored to consider in the EVPPI
  
}
  
#  Costs

cPrimary <- 0
mn.cRevision <- 5294
se.cRevision <- 1487
a.cRevision <- (mn.cRevision/se.cRevision)^2
b.cRevision <- (se.cRevision^2)/mn.cRevision
cRevision <- rgamma(sim.runs,shape=a.cRevision,scale=b.cRevision)
cSuccess <- rep(0, sim.runs)
cDead <- rep(0, sim.runs)

# cost of prostheses
cSP0 <- 394
cNP1 <- 579

state.costs<-data.frame(cPrimary, cSuccess, cRevision,cSuccess,cDead)


#  Utilities

mn.uSuccessP<-0.85
se.uSuccessP<-0.03
ab.uSuccessP<-mn.uSuccessP*(1-mn.uSuccessP)/(se.uSuccessP^2)
a.uSuccessP<-mn.uSuccessP*ab.uSuccessP
b.uSuccessP<-a.uSuccessP*(1-mn.uSuccessP)/mn.uSuccessP
uSuccessP<-rbeta(sim.runs,a.uSuccessP,b.uSuccessP)

mn.uSuccessR<-0.75
se.uSuccessR<-0.04
ab.uSuccessR<-mn.uSuccessR*(1-mn.uSuccessR)/(se.uSuccessR^2)
a.uSuccessR<-mn.uSuccessR*ab.uSuccessR
b.uSuccessR<-a.uSuccessR*(1-mn.uSuccessR)/mn.uSuccessR
uSuccessR<-rbeta(sim.runs,a.uSuccessR,b.uSuccessR)

mn.uRevision<-0.30
se.uRevision<-0.03
ab.uRevision<-mn.uRevision*(1-mn.uRevision)/(se.uRevision^2)
a.uRevision<-mn.uRevision*ab.uRevision
b.uRevision<-a.uRevision*(1-mn.uRevision)/mn.uRevision
uRevision<-rbeta(sim.runs,a.uRevision,b.uRevision)

uPrimary <- rep(0, sim.runs)
uDead <- rep(0, sim.runs)

state.utilities <- data.frame(uPrimary,uSuccessP,uRevision,uSuccessR,uDead)



#  Create revision and death risk as function of age

mr <- 3-male
cycle <- 1:cycles
current.age <- age+cycle

death.risk <- data.table(current.age)
setkey(life.table,"Index")
setkey(death.risk,"current.age")
death.risk <- life.table[death.risk, roll=TRUE]
death.risk.vector <- as.vector(as.matrix(death.risk[,..mr]))

# Parameters that need to be passed into the model 
  # 1 revision risks
  # 2 death rates 
  # 3 state utilities
  # 4 state costs
  # 5 transitions
  
  # revision.vec <- revision.LP[1,]
  # utilities.vec <- as.numeric(state.utilities[1,])
  # state.costs.vec <- as.numeric(state.costs[1,])
  # transition.vec <- as.numeric(transitions[1,])
  
# need to call the model appropraitely 

run.model <- function(revision = revision.vec, death = death.risk.vector, 
                      utilities = utilities.vec, state.costs = state.costs.vec,
                      transition = transition.vec) { # pass the model the revision paramaters / and others???
  
  # unpack the data from data.frames
  sp0.LP <- revision[1]
  np1.LP <- revision[2]
  lngamma <- revision[3]
  
  omrPTHR <- transition[1]
  omrRTHR <- transition[2]
  rrr <- transition[3]
    
  # generate time dependent revision risks
      
  revision.risk.sp0<-1-exp(exp(sp0.LP)*((cycle-1)^exp(lngamma)-cycle^exp(lngamma)))
  revision.risk.np1<-1-exp(exp(np1.LP)*((cycle-1)^exp(lngamma)-cycle^exp(lngamma)))
  
  
  SP0.tm<-array(data=0,dim=c(5,5,60))

    # vectorisation for speed
    SP0.tm[1,2,]<-1-omrPTHR
    SP0.tm[1,5,]<-omrPTHR
    SP0.tm[2,2,]<-1 - revision.risk.sp0 - death
    SP0.tm[2,3,]<-revision.risk.sp0
    SP0.tm[2,5,]<-death
    SP0.tm[3,4,]<-1-omrRTHR-death
    SP0.tm[3,5,]<-omrRTHR+death
    SP0.tm[4,3,]<-rrr
    SP0.tm[4,4,]<-1-rrr-death
    SP0.tm[4,5,]<-death
    SP0.tm[5,5,]<-1


  NP1.tm<-array(data=0,dim=c(5,5,60))

    # vectorisation for speed
    NP1.tm[1,2,]<-1-omrPTHR
    NP1.tm[1,5,]<-omrPTHR
    NP1.tm[2,2,]<-1-revision.risk.np1-death
    NP1.tm[2,3,]<-revision.risk.np1
    NP1.tm[2,5,]<-death
    NP1.tm[3,4,]<-1-omrRTHR-death
    NP1.tm[3,5,]<-omrRTHR+death
    NP1.tm[4,3,]<-rrr
    NP1.tm[4,4,]<-1-rrr-death
    NP1.tm[4,5,]<-death
    NP1.tm[5,5,]<-1

  #  Create a trace for the standard prosthesis arm
  
  trace.SP0<-matrix(data=NA,nrow=cycles,ncol=n.states)
  colnames(trace.SP0)<-state.names
  trace.SP0[1,]<-seed%*%SP0.tm[,,1]
  
  for (i in 2:cycles) trace.SP0[i,]<-trace.SP0[i-1,]%*%SP0.tm[,,i]

  #  Create a trace for the new prosthesis arm
  
  trace.NP1<-matrix(data=NA,nrow=cycles,ncol=n.states)
  colnames(trace.NP1)<-state.names
  trace.NP1[1,]<-seed%*%NP1.tm[,,1]
  
  for (i in 2:cycles) trace.NP1[i,]<-trace.NP1[i-1,]%*%NP1.tm[,,i]

  #  Calculate QALYs & discounted QALYs in each treatment arm
  
  QALYs.SP0<-trace.SP0%*%as.numeric(utilities)
  QALYs.NP1<-trace.NP1%*%utilities

  undisc.QALYs.SP0<-colSums(QALYs.SP0)
  undisc.QALYs.NP1<-colSums(QALYs.NP1)

  disc.QALYs.SP0<-O.discount.factor%*%QALYs.SP0
  disc.QALYs.NP1<-O.discount.factor%*%QALYs.NP1

  
  cost.SP0<-trace.SP0%*%state.costs
  undisc.cost.SP0<-colSums(cost.SP0)+cSP0

  cost.NP1<-trace.NP1%*%state.costs
  undisc.cost.NP1<-colSums(cost.NP1)+cNP1

  disc.cost.SP0<-C.discount.factor%*%cost.SP0+cSP0
  disc.cost.NP1<-C.discount.factor%*%cost.NP1+cNP1
  
  
  #  Incremetnal cost-effectiveness results
  
  inc.cost<-disc.cost.NP1-disc.cost.SP0
  inc.QALYs<-disc.QALYs.NP1-disc.QALYs.SP0

  
  increments<-c(QALYs.SP0 = disc.QALYs.SP0, cost.SP0 = disc.cost.SP0,
                QALYs.NP1 = disc.QALYs.NP1, cost.NP1 = disc.cost.NP1)
  
  return(increments)
  
}





###  PSA  ### 

# The PSA is more complex now, as we need to feed to correct data samples into the model for each run  




# Use a loop to run simulations

# Set the number of simulations to run

simulation.results <- data.frame(matrix(0, sim.runs, 4))
colnames(simulation.results) <- c("QALY SP0", "Cost SP0", "QALY NP1", "Cost NP1")
pb = txtProgressBar(min = 0, max = sim.runs, initial = 0, style = 3)

for(i in 1:sim.runs) {
  
  # In each of the model simulatuions, the correct parameters for this simulation is selected from all parameter samples 
  
  revision.vec <- revision.LP[i,]
  utilities.vec <- as.numeric(state.utilities[i,])
  state.costs.vec <- as.numeric(state.costs[i,])
  transition.vec <- as.numeric(transitions[i,])
  
  # Run the model and save results 
  simulation.results[i,] <- run.model()
  
  # Update the progress bar 
  setTxtProgressBar(pb,i) 
  
}


# Mean results 
colMeans(simulation.results)

incremental.results <- data.frame(matrix(0, nrow = sim.runs, ncol = 2))
incremental.results[,1] <- simulation.results[,3] - simulation.results[,1]
incremental.results[,2] <- simulation.results[,4] - simulation.results[,2]
colnames(incremental.results) <- c("inc. QALYs", "inc. Costs")

## CEAC and EVPI tables 


# CEAC table

pCE<-function(lambda, results = incremental.results) {
  
  nmb <- results[,1]*lambda - results[,2] 
  CE <- nmb>0
  probCE<- mean(CE)
  
  return(probCE)
  
}

lambda.values <- seq(from = 0, to = 50000, by = 100)
CEAC <- data.frame(matrix(data = NA, nrow = length(lambda.values), ncol = 2))
colnames(CEAC)<-c("lambda","pCE")


for (i in 1:length(lambda.values)) {
  CEAC[i,1] <- lambda.values[i]
  CEAC[i,2]<- pCE(lambda.values[i], incremental.results)
}



# EVPI table

population <- 40000 
years <- 10
evpi.disc <- 0.06

population.seq <- sum(population * (1/(1+evpi.disc) ^ c(0:(years-1))))
effective.population <- sum(population.seq)


est.EVPI <-function(lambda, results = simulation.results) {
  
  nmb.SP0 <- ((results[,1] * lambda) - results[,2])  
  nmb.NP1 <- ((results[,3] * lambda) - results[,4]) 
  nmb <- data.frame(nmb.SP0, nmb.NP1)
  av.nmb <- apply(nmb, 2, mean)
  
  max.nmb.sample <- apply(nmb, 1, max)
  av.nmb.max <- mean(max.nmb.sample)
  
  EVPI <- av.nmb.max - max(av.nmb)  
  
  return(EVPI)
  
}

EVPI.patient <- data.frame(matrix(data = NA, nrow = length(lambda.values), ncol = 2))
colnames(EVPI.patient) <- c("lambda", "EVPI per patient")

for (i in 1:length(lambda.values)) {
  EVPI.patient[i,1] <- lambda.values[i]
  EVPI.patient[i,2]<-  est.EVPI(lambda.values[i], simulation.results)
}


EVPI.population <- EVPI.patient
EVPI.population[,2] <- EVPI.patient[,2] * effective.population 


# Plots 


# ggplot function 

ce.plane(incremental.results)

plot.ceac(CEAC)

plot.evpi(EVPI.patient)

plot.evpi(EVPI.population)





## EVPPI - TBC ? 

## Enter inner and outer loop numbers - note these must be higher than sim.runs 
inner.loops <- 200
outer.loops <- 200



# generate matrices to store results 

inner.results <- matrix(0, inner.loops, 4)
colnames(inner.results) <- c("QALY SP0", "Cost SP0", "QALY NP1", "Cost NP1")

evppi.results.SP0 <- matrix(0, ncol = length(lambda.values), nrow = outer.loops)
colnames(evppi.results.SP0) <- as.character(lambda.values)
evppi.results.NP1 <- evppi.results.SP0 


# estimate NMB across all the lambda values 
nmb.function <- function(lambda, results){
  
  nmb.table <- matrix(lambda, ncol = length(lambda), nrow = dim(results)[1],  byrow = TRUE) 
  
  SP0 <- ((results[,1] * nmb.table) - results[,2])  
  NP1 <- ((results[,3] * nmb.table) - results[,4])
  
  nmb.p <- apply(SP0, 2, mean)
  nmb.t <- apply(NP1, 2, mean) 


  return(list(nmb.t, nmb.p))
  
}


## Function to estimate EVPPI from outputs


gen.evppi.results <- function(evppi.results1 = evppi.results.SP0, evppi.results2 = evppi.results.NP1, lambda = lambda.values){
  
  ## calculate the mean NMB for placebo and txa, at each lambda 
  current.info1 <- apply(evppi.results1, 2, mean)
  current.info2 <- apply(evppi.results2, 2, mean)
  
  current.info <- pmax(current.info1, current.info2)
  
  evppi.array <- array(0, dim = c(outer.loops, length(lambda), 2))
  evppi.array[,,1] <- evppi.results1
  evppi.array[,,2] <- evppi.results2
  
  perf.info.sims <- apply(evppi.array, c(1,2), max)
  perf.info <- apply(perf.info.sims, 2, mean)
  
  evppi.results <- c(perf.info - current.info)
  
  evppi <- data.frame(lambda, evppi.results)
  
  return(evppi)
  
}





# EVPPI Sampling

## sample all parameters

# revision.vec <- revision.LP[1,]
# utilities.vec <- as.numeric(state.utilities[1,])
# state.costs.vec <- as.numeric(state.costs[1,])
# transition.vec <- as.numeric(transitions[1,])
# revision.LP[1,] 





## EVPPI loops - NP1 parameter (excl survial parameters)

pb = txtProgressBar(min = 0, max = outer.loops, initial = 0, style = 3)

for(a in 1:outer.loops){
  
  ## 1. Select the 'partial' parameter from the outer loop 
  
  revision.vec <- revision.LP[a,]
  
  
  for(b in 1:inner.loops){
    
    # 2. Select traditional parameters, minus the outer loop parameter
    
    # revision.vec <- revision.LP[a,]
    utilities.vec <- as.numeric(state.utilities[b,])
    state.costs.vec <- as.numeric(state.costs[b,])
    transition.vec <- as.numeric(transitions[b,])
    revision.vec[1] <- revision.LP[b,1]
    revision.vec[3] <- revision.LP[b,3]
    revision.vec[2] <- revision.LP[b,1] + revision.LP[a,4] # NP1 parameter only changes on outer loop (so select 'a' row)
    
    inner.results[b,] <- run.model() 
    
  }
  
  #after each inner loop PSA, calculate the mean NMB for each tx and store the results
  nmb <- nmb.function(lambda.values, inner.results)
  
  evppi.results.SP0[a,] <- nmb[[1]]
  evppi.results.NP1[a,] <- nmb[[2]]
  
  setTxtProgressBar(pb,a)
}

NP1.evppi <- gen.evppi.results()





## EVPPI loops - EVPPI for survival parameters

pb = txtProgressBar(min = 0, max = outer.loops, initial = 0, style = 3)

for(a in 1:outer.loops){
  
  ## 1. Select the 'partial' parameter from the outer loop 
  
  revision.vec <- revision.LP[a,]
  
  
  for(b in 1:inner.loops){
    
    # 2. Select traditional parameters, minus the outer loop parameter
    
    # revision.vec <- revision.LP[a,]
    utilities.vec <- as.numeric(state.utilities[b,])
    state.costs.vec <- as.numeric(state.costs[b,])
    transition.vec <- as.numeric(transitions[b,])
    #revision.vec[1] <- revision.LP[a,1] - this has been done in the outer loop
    #revision.vec[3] <- revision.LP[a,3]
    revision.vec[2] <- revision.LP[a,1] + revision.LP[b,4] # NP1 parameter is in the inner loop (not in EVPPI)
    
    inner.results[b,] <- run.model() 
    
  }
  
  #after each inner loop PSA, calculate the mean NMB for each tx and store the results
  nmb <- nmb.function(lambda.values, inner.results)
  
  evppi.results.SP0[a,] <- nmb[[1]]
  evppi.results.NP1[a,] <- nmb[[2]]
  
  setTxtProgressBar(pb,a)
}

surv.evppi <- gen.evppi.results()





## EVPPI loops - Utilities

pb = txtProgressBar(min = 0, max = outer.loops, initial = 0, style = 3)

for(a in 1:outer.loops){
  
  ## 1. Select the 'partial' parameter from the outer loop 
  
  utilities.vec <- as.numeric(state.utilities[a,])
  
  for(b in 1:inner.loops){
    
    # 2. Select traditional parameters, minus the outer loop parameter
    
    revision.vec <- revision.LP[b,]
    #utilities.vec <- as.numeric(state.utilities[b,])
    state.costs.vec <- as.numeric(state.costs[b,])
    transition.vec <- as.numeric(transitions[b,])
    
    
    inner.results[b,] <- run.model() 
    
  }
  
  #after each inner loop PSA, calculate the mean NMB for each tx and store the results
  nmb <- nmb.function(lambda.values, inner.results)
  
  evppi.results.SP0[a,] <- nmb[[1]]
  evppi.results.NP1[a,] <- nmb[[2]]
  
  setTxtProgressBar(pb,a)
}

utilities.evppi <- gen.evppi.results()






## EVPPI loops - State costs - cost revision

pb = txtProgressBar(min = 0, max = outer.loops, initial = 0, style = 3)

for(a in 1:outer.loops){
  
  ## 1. Select the 'partial' parameter from the outer loop 
  
  state.costs.vec <- as.numeric(state.costs[a,])
  
  for(b in 1:inner.loops){
    
    # 2. Select traditional parameters, minus the outer loop parameter
    
    revision.vec <- revision.LP[b,]
    utilities.vec <- as.numeric(state.utilities[b,])
    #state.costs.vec <- as.numeric(state.costs[b,])
    transition.vec <- as.numeric(transitions[b,])
    
    
    inner.results[b,] <- run.model() 
    
  }
  
  #after each inner loop PSA, calculate the mean NMB for each tx and store the results
  nmb <- nmb.function(lambda.values, inner.results)
  
  evppi.results.SP0[a,] <- nmb[[1]]
  evppi.results.NP1[a,] <- nmb[[2]]
  
  setTxtProgressBar(pb,a)
}

cRevision.evppi <- gen.evppi.results()





## EVPPI loops - State costs - transition parameters (OMRs)

pb = txtProgressBar(min = 0, max = outer.loops, initial = 0, style = 3)

for(a in 1:outer.loops){
  
  ## 1. Select the 'partial' parameter from the outer loop 
  
  transition.vec <- as.numeric(transitions[a,])
  
  for(b in 1:inner.loops){
    
    # 2. Select traditional parameters, minus the outer loop parameter
    
    revision.vec <- revision.LP[b,]
    utilities.vec <- as.numeric(state.utilities[b,])
    state.costs.vec <- as.numeric(state.costs[b,])
    #transition.vec <- as.numeric(transitions[b,])
    transition.vec[3] <- as.numeric(transitions[b,3]) # overwrite the RRR value so included in PSA
    
    inner.results[b,] <- run.model() 
    
  }
  
  #after each inner loop PSA, calculate the mean NMB for each tx and store the results
  nmb <- nmb.function(lambda.values, inner.results)
  
  evppi.results.SP0[a,] <- nmb[[1]]
  evppi.results.NP1[a,] <- nmb[[2]]
  
  setTxtProgressBar(pb,a)
}

omr.evppi <- gen.evppi.results()





## EVPPI loops - State costs - transition parameters (Re-revision risk)

pb = txtProgressBar(min = 0, max = outer.loops, initial = 0, style = 3)

for(a in 1:outer.loops){
  
  ## 1. Select the 'partial' parameter from the outer loop 
  
  transition.vec <- as.numeric(transitions[a,])
  
  for(b in 1:inner.loops){
    
    # 2. Select traditional parameters, minus the outer loop parameter
    
    revision.vec <- revision.LP[b,]
    utilities.vec <- as.numeric(state.utilities[b,])
    state.costs.vec <- as.numeric(state.costs[b,])
    #transition.vec <- as.numeric(transitions[b,])
    transition.vec[1:2] <- as.numeric(transitions[b,1:2]) # overwrite the OMR values within inner loop
    
    inner.results[b,] <- run.model() 
    
  }
  
  #after each inner loop PSA, calculate the mean NMB for each tx and store the results
  nmb <- nmb.function(lambda.values, inner.results)
  
  evppi.results.SP0[a,] <- nmb[[1]]
  evppi.results.NP1[a,] <- nmb[[2]]
  
  setTxtProgressBar(pb,a)
}

rrr.evppi <- gen.evppi.results()




# Create a data frame with all EVPPI results

evppi.wide.patient <- data.frame(NP1.evppi,
                                 surv.evppi[,2],
                                 utilities.evppi[,2],
                                 cRevision.evppi[,2],
                                 omr.evppi[,2],
                                 rrr.evppi[,2])

colnames(evppi.wide.patient) <- c("lambda", "NP1 parameter", "Survival parameters", "Utilities", "Revision cost", "Operative mortality ratios", "Re-revision risk")

# Transform from per patient EVPPI to population EVPPI 

evppi.wide.population <- evppi.wide.patient * effective.population
evppi.wide.population[,1] <- evppi.wide.patient[,1]

evppi.long.population <- reshape2::melt(evppi.wide.population, id.vars = c("lambda"))


## Plot

plot.evppi(evppi.long.population)

plot.evppi(evppi.long.population, xlimit = 10000) # Limit plot to 10,000 on x axis






