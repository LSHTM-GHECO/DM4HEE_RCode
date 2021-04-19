#  Decision Modelling for Health Economic Evaluation
#  Advanced Course Exercise 4a (Part 1): TEMPLATE FILE
#  Authors: Andrew Briggs, Jack Williams & Nichola Naylor

### !!! to do

# Load packages 
library(data.table)


# Run pre-set functions for ggplot graphics 
source('graphs/ggplot functions.R')


# Define the number of model simulations (for PSA) 
sim.runs <- 1000


#########**** PARAMETERS *****######

#  Demographics
age <- 60
male <- 0

# Discount rates
dr.c <- 0.06
dr.o <- 0.015




#  Read in the life table and covariance table
life.table <- read.csv("inputs/life-table.csv")
life.table <- data.table(life.table)
cov.55 <- read.csv("inputs/cov55.csv",row.names=1,header=TRUE) 


#  Set the total number of cycles to run and name the states
cycles <- 60


state.names <- c("P-THR","successP-THR","R-THR","successR-THR","Death")
n.states <- length(state.names)

#  Seed the starting states of the model (cycle 0)
seed <- c(1,0,0,0,0)




# Discounting matrices

O.discount.factor <- matrix(1/(1+dr.o) ^ c(1:cycles), nrow = 1, ncol = cycles)
C.discount.factor <- matrix(1/(1+dr.c) ^ c(1:cycles), nrow = 1, ncol = cycles)


#  Defining parameters


#  Transition probabilities
  
omrPTHR <- rbeta(sim.runs,2,98)
omrRTHR <- rbeta(sim.runs,2,98)
rrr <- rbeta(sim.runs,4,96)
  
# create transitions data.frame to pass into the model function
transitions <- data.frame(omrPTHR = omrPTHR, omrRTHR = omrRTHR, rrr = rrr)
  

#### HAZARD FUNCTION & ASSOCIATED PARAMETERS #####

hazards <- read.csv("inputs/hazardfunction.csv") ## importing the hazard inputs from the regression analysis

r.lnlambda <- hazards$coefficient[1] ## Ancilliary parameter in Weibull distribution - equivalent to lngamma coefficient
r.cons<- hazards$coefficient[2] ##Constant in survival analysis for baseline hazard
r.ageC<- hazards$coefficient[3] ## Age coefficient in survival analysis for baseline hazard
r.maleC<- hazards$coefficient[4] ## Male coefficient in survival analysis for baseline hazard
r.NP1<- hazards$coefficient[5]
  
# mn.cons <- -5.490935
# mn.ageC <- -0.0367022
# mn.maleC <- 0.768536
# mn.NP1 <- -1.344474
# mn.lngamma <- 0.3740968

# Need to amend below to match naming convention

mn <- c(r.lnlambda, r.cons, r.ageC, r.maleC, r.NP1)
  
cholm <- t(chol(t(cov.55))) 
  
z.mat <- matrix(rnorm(sim.runs*5, 0, 1), nrow = sim.runs, ncol = 5)
  
revision.LP <- matrix(NA, nrow = sim.runs, ncol = 4)
colnames(revision.LP) <- c("standard", "NP1", "lngamma", "NP1")

for(i in 1:sim.runs){
    
  x <- mn + cholm %*% z.mat[i,]
  
  lngamma <- x[1,1]
  cons <- x[2,1]
  ageC <- x[3,1]
  maleC <- x[4,1]
  NP1 <- x[5,1]
  
  revision.LP[i,1] <- cons+age*ageC+male*maleC
  revision.LP[i,2] <- cons+age*ageC+male*maleC+NP1
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

col.key <- 4-male
cycle.v <- 1:cycles
current.age <- age+cycle.v

death.risk <- data.table(current.age)
setkey(life.table,"Index")
setkey(death.risk,"current.age")
death.risk <- life.table[death.risk, roll=TRUE]
death.risk.vector <- as.vector(as.matrix(death.risk[,..col.key]))

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

model.THR <- function(revision = revision.vec, death = death.risk.vector, 
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
      
  revision.risk.sp0<-1-exp(exp(sp0.LP)*((cycle.v-1)^exp(lngamma)-cycle.v^exp(lngamma)))
  revision.risk.np1<-1-exp(exp(np1.LP)*((cycle.v-1)^exp(lngamma)-cycle.v^exp(lngamma)))
  
  
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
  simulation.results[i,] <- model.THR()
  
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



