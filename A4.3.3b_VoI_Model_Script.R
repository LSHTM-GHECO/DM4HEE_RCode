#  Decision Modelling for Health Economic Evaluation
#  Advanced Course Exercise 3a (Part 1): SOLUTION FILE
#  Authors: Andrew Briggs, Jack Williams & Nichola Naylor

### Loading useful packages
library(ggplot2)
library(reshape2) 

#  Reading the data needed from csv files
hazards <- read.csv("hazardfunction.csv", header=TRUE) ## importing the hazard inputs from the regression analysis
cov.55 <- read.csv("cov55.csv",row.names=1,header=TRUE) ## importing the covariance matrix
life.table <- read.csv("life-table.csv", header=TRUE)

# Setting sampling numbers
sim.runs <- 10000 ## the number of simulation runs 
## ! note - this needs to be more than or equal to 
## the number of inner and outer runs in VoI scripts
## see "A4.3.3b_Value_of_Information_Part 2_Solution.R" to see their definition

#########**** PARAMETERS *****######

age <- 60
male <- 0

# SETTING CONSTANT PARAMETERS OUTSIDE THE FUNCTION 

##### DETERMINISTIC PARAMETERS ######
## this section is the same as Exercise 3
## head to the "PROBABLISTIC PARAMETERS" section to see the 
## start of the changes needed to perform EVPPI

dr.c <- 0.06 ## set the discount rate for costs (6%)
dr.o <- 0.015 ## set the discount rate for outcomes (15%)
cycles <- 60 ## number of cycles running the model
state.names <- c("P-THR","successP-THR","R-THR","successR-THR","Death") ## a vector of state names
n.states <- length(state.names) ## number of states in the model
seed <- c(1,0,0,0,0) #  Seed the starting states of the model (a vector of 1 and 0s for this exercise)

## Cost of standard prosthesis and new prosthesis
c.SP0 <- 394 ## Cost of standard prosthesis
c.NP1 <- 579 ## Cost of new prosthesis 1

### alpha and beta values
# FOR TRANSITION PROBABILITY ESTIMATION
a.PTHR2dead <- 2 ## alpha value for operative mortality from primary surgery
b.PTHR2dead <- 100- a.PTHR2dead ## beta value for operative mortality from primary surgery

a.rrr <- 4 ## alpha value for revision risk 
b.rrr <- 100-a.rrr ## beta value for revision risk

# HAZARD FUNCTION PARAMETERS

## Coefficients - on the log hazard scale
mn.lngamma <- hazards$coefficient[1] ## Ancilliary parameter in Weibull distribution - equivalent to lngamma coefficient
mn.cons <- hazards$coefficient[2] ##Constant in survival analysis for baseline hazard
mn.ageC <- hazards$coefficient[3] ## Age coefficient in survival analysis for baseline hazard
mn.maleC <- hazards$coefficient[4] ## Male coefficient in survival analysis for baseline hazard
mn.NP1 <- hazards$coefficient[5]
mn<-c(mn.lngamma, mn.cons,mn.ageC,mn.maleC,mn.NP1) ## vector of mean values from the regression analysis

cholm <- t(chol(t(cov.55))) ## lower triangle of the Cholesky decomposition

# FOR COST ESTIMATION
c.primary <- 0  ## Cost of a primary THR procedure - 
## Note that the cost of the primary procedure is excluded (set to 0): since both arms have this procedure it is assumed to net out of the incremental analysis.  However, if the model was to be used to estimate lifetime costs of THR it would be important to include.
mn.cRevision <- 5294 ## mean cost of revision surgery
se.cRevision <- 1487 ## standard error of cost of revision surgery
a.cRevision <- (mn.cRevision/se.cRevision)^2 ## alpha value for cost of revision surgery 
b.cRevision <- (se.cRevision^2)/mn.cRevision ## beta value for cost of revision surgery
c.success <- 0 ## Cost of one cycle in a 'success' state (primary or revision)

# FOR UTILITY ESTIMATION
## during the revision period 
mn.uSuccessP <- 0.85 ## mean utility value for successful primary prosthesis
se.uSuccessP <- 0.03 ## standard errror utility value for successful primary prosthesis
ab.uSuccessP <- mn.uSuccessP*(1-mn.uSuccessP)/(se.uSuccessP^2)-1 ## estimating alpha plus beta (ab)
a.uSuccessP<-mn.uSuccessP*ab.uSuccessP ## estimating alpha (a)
b.uSuccessP<-a.uSuccessP*(1-mn.uSuccessP)/mn.uSuccessP ## estimating beta (b)

mn.uSuccessR<-0.75 ## mean utility value for having a successful Revision THR
se.uSuccessR<-0.04 ## standard error utility value for having a successful Revision THR
ab.uSuccessR<-mn.uSuccessR*(1-mn.uSuccessR)/(se.uSuccessR^2)-1 ## alpha + beta (ab)
a.uSuccessR<-mn.uSuccessR*ab.uSuccessR ## alpha (a)
b.uSuccessR<-a.uSuccessR*(1-mn.uSuccessR)/mn.uSuccessR ## beta(b)

mn.uRevision<-0.30 ## mean utility score during the revision period
se.uRevision<-0.03 ## standard error utility score during the revision period
ab.uRevision<-mn.uRevision*(1-mn.uRevision)/(se.uRevision^2)-1 ## alpha + beta (ab)
a.uRevision<-mn.uRevision*ab.uRevision ## alpha (a)
b.uRevision<-a.uRevision*(1-mn.uRevision)/mn.uRevision ## beta(b)

## discount matrices
cycle.v <- 1:cycles ## a vector of cycle numbers 1 - 60
discount.factor.c <- 1/(1+dr.c)^cycle.v ## the discount factor matrix
discount.factor.o <- 1/(1+dr.o)^cycle.v  ## discount factor matrix for utility 

## LIFE TABLE DATA  - outside of the model when age and sex are specified upfront 
# This is included within the function as it varies by age and sex (which are inputs into the function)
colnames(life.table) <- c("Age","Index","Males","Female") ## making sure column names are correct
current.age <- age + cycle.v ## a vector of cohort age throughout the model
interval <- findInterval(current.age, life.table$Index)
death.risk <- data.frame(age = current.age, males = life.table[interval,3], females = life.table[interval,4])
col.key <- 3-male 
mortality.vec <- death.risk[,col.key]

#########**** PROBABILISTIC PARAMETERS *****######

# we now want to create vectors & data.frames of the probablistic values:
###  Transition probabilities
tp.PTHR2dead <- rbeta(sim.runs, a.PTHR2dead, b.PTHR2dead) ## OMR following primary THR
tp.RTHR2dead <- rbeta(sim.runs, a.PTHR2dead, b.PTHR2dead)  ## OMR following revision THR
## creating a data.frame of sampled transition probabilites
omr.df <- data.frame(tp.PTHR2dead, tp.RTHR2dead) 

tp.rrr.vec <-rbeta(sim.runs, a.rrr, b.rrr) ## Re-revision risk 

##  Costs
c.revision.vec <- rgamma(sim.runs, shape=a.cRevision, scale=b.cRevision) ## Gamma distribution draw for cost of revision surgery
## since this is the only probablistic cost sampled we can leave as a vector

##  Utilities
uSuccessP <- rbeta(sim.runs, a.uSuccessP, b.uSuccessP) 
uSuccessR <- rbeta(sim.runs, a.uSuccessR, b.uSuccessR) 
uRevision <- rbeta(sim.runs, a.uRevision, b.uRevision) 
## Make a data frame to pass into the function
state.utilities.df <- data.frame(uprimary=rep(0, sim.runs), 
                              uSuccessP, uRevision, uSuccessR,
                              udeath=rep(0, sim.runs))

##  Hazard function ####
## We now need to perform the same cholesky decomposition
## and sampling to create a hazard function data.frame to be passed
## through the model
z <- matrix(rnorm(5*sim.runs, 0, 1), nrow = sim.runs, ncol = 5) ## 5 random draws, by sim.runs
r.table <- matrix(0, nrow = sim.runs, ncol = 5)
colnames(r.table) <- c("lngamma", "cons", "age", "male", "NP1")

for(i in 1:sim.runs){

  Tz <- cholm %*% z[i,] 
  x <- mn + Tz 
  r.table[i,] <- x[,1]
  
}

r <- as.data.frame(r.table)
gamma.vec <- exp(r$lngamma)
lambda.vec <- exp(r$cons + age * r$age + male*r$male)
RR.vec <- exp(r$NP1)

## creating a data.frame with the parameters
## taken from the hazard function analysis
survival.df <- data.frame(gamma.vec,lambda.vec)
# note we just use the vector for rr.NP1 (since we want to investigate) this parameter specifically

## lets take a look at what we've created so far:
# parameter groups: (data.frames)
head(survival.df)
head(state.utilities.df)
head(omr.df)

# individual parameter values: (vectors)
head(tp.rrr.vec)
head(RR.vec)
head(c.revision.vec)


### now we define an adjusted model function:
model.THR.voi <- function(RR.NP1, ## from RR.vec
                          omr,  ## from omr.df
                          tp.rrr, ## from tp.rrr.vector
                          survival, ## from survival.df
                          c.revision, ## from c.revision.vector
                          state.util) { ## from state.utilities.df
 ### FUNCTION: Run EVPPI on probablistic parameters wihin the THR model
  ##           on 6 pre-specified parameter/parameter groups
  ## INPUTS: 3 data.frame rows (1 row from omr.df, survival.df & state.utilities.df) 
  ##        and 3 vector values (1 value from vectors; c.revision.vector, tp.rrr.vector, RR.vec)
  ## OUTPUTS: 
  
  ## First, need to unpack values from the data.frames provided to the function 
  
  tp.PTHR2dead <- unlist(omr[1,1])
  tp.RTHR2dead <- unlist(omr[1,2])
  
  state.utilities <- unlist(state.util)
  
  gamma <- unlist(survival[1,1])
  lambda <- unlist(survival[1,2])
  
  
  ## Make sure the revision cost passed into function gets added into state cost vector
  state.costs<-c(c.primary, c.success, c.revision, c.success, 0) ## a vector with the costs for each state

  ## Now we continue on with our previous model.THR calculations:
  
  # Calculate revision risks 
  revision.risk.sp0 <- 1- exp(lambda * ((cycle.v-1) ^gamma-cycle.v ^gamma))
  revision.risk.np1 <- 1- exp(lambda * RR.NP1 * ((cycle.v-1) ^gamma-cycle.v ^gamma))

  
  # Transition arrays
  tm.SP0 <- array(data=0,dim=c(n.states, n.states, cycles),
                  dimnames= list(state.names, state.names, 1:cycles)) ## an empty array of dimenions (number of states, number of states, number of cycles)

  # Now using vectorisation to complete this (i.e. not using the loop, to help speed this up)

    tm.SP0["P-THR","Death",] <- tp.PTHR2dead 
    tm.SP0["P-THR","successP-THR",] <- 1 - tp.PTHR2dead 
    tm.SP0["successP-THR","R-THR",] <- revision.risk.sp0 
    tm.SP0["successP-THR","Death",] <- mortality.vec
    tm.SP0["successP-THR","successP-THR",] <- 1 - revision.risk.sp0 - mortality.vec
    tm.SP0["R-THR","Death",] <- tp.RTHR2dead + mortality.vec
    tm.SP0["R-THR","successR-THR",] <- 1 - tp.RTHR2dead - mortality.vec
    tm.SP0["successR-THR","R-THR",] <- tp.rrr
    tm.SP0["successR-THR","Death",] <- mortality.vec
    tm.SP0["successR-THR","successR-THR",] <- 1 - tp.rrr - mortality.vec
    tm.SP0["Death","Death",] <- 1 

  #  Create a trace for the standard prosthesis arm
  trace.SP0 <- matrix(data=0, nrow=cycles, ncol=n.states)
  colnames(trace.SP0) <- state.names
  
  trace.SP0[1,] <- seed%*%tm.SP0[,,1]
  
  for (i in 2:cycles) trace.SP0[i,] <- trace.SP0[i-1,]%*%tm.SP0[,,i]

  #  NP1 ARM
  tm.NP1 <- array(data=0,dim=c(n.states, n.states, cycles),
                  dimnames= list(state.names, state.names, 1:cycles)) ## an empty array of dimenions (number of states, number of states, number of cycles)
  ## naming all dimensions
  
  ### create a loop that creates a time dependent transition matrix for each cycle
    tm.NP1["P-THR","Death",] <- tp.PTHR2dead ## Primary THR either enter the death state or.. or..
    tm.NP1["P-THR","successP-THR",] <- 1 - tp.PTHR2dead ## they go into the success THR state 
    tm.NP1["successP-THR","R-THR",] <- revision.risk.np1 ## revision risk with NP1 treatment arm 
    tm.NP1["successP-THR","Death",] <- mortality.vec
    tm.NP1["successP-THR","successP-THR",] <- 1 - revision.risk.np1 - mortality.vec
    tm.NP1["R-THR","Death",] <- tp.RTHR2dead + mortality.vec
    tm.NP1["R-THR","successR-THR",] <- 1 - tp.RTHR2dead - mortality.vec
    tm.NP1["successR-THR","R-THR",] <- tp.rrr
    tm.NP1["successR-THR","Death",] <- mortality.vec[i]
    tm.NP1["successR-THR","successR-THR",] <- 1 - tp.rrr - mortality.vec
    tm.NP1["Death","Death",] <- 1 


  #  Create a trace for the standard prosthesis arm
  trace.NP1 <- matrix(data=0,nrow=cycles,ncol=n.states)
  colnames(trace.NP1) <- state.names
  
  trace.NP1[1,] <- seed%*%tm.NP1[,,1]
  
  for (i in 2:cycles) trace.NP1[i,] <- trace.NP1[i-1,]%*%tm.NP1[,,i]

  # COST #

  cost.SP0 <- trace.SP0%*%state.costs  
  disc.cost.SP0 <- (discount.factor.c%*%cost.SP0) + c.SP0   

  cost.NP1 <- trace.NP1%*%state.costs  
  disc.cost.NP1 <- (discount.factor.c%*%cost.NP1) + c.NP1 
  
  # QALYS #
  QALYs.SP0 <- trace.SP0%*%state.utilities ## utility per cycle
  disc.QALYs.SP0 <- colSums(discount.factor.o%*%QALYs.SP0) ## total discounted utility
  
  QALYs.NP1 <- trace.NP1%*%state.utilities ## utility per cycle
  disc.QALYs.NP1 <- colSums(discount.factor.o%*%QALYs.NP1) ## total discounted utility


  output <- c(cost.SP0 = disc.cost.SP0,
              qalys.SP0 = disc.QALYs.SP0,
              cost.NP1 = disc.cost.NP1,
              qalys.NP1 = disc.QALYs.NP1)
  
  return(output)

}

## testing the function:
i = 1
model.THR.voi(RR.vec[i], omr.df[i,],  tp.rrr.vec[i], 
              survival.df[i,],c.revision.vec[i], 
              state.utilities.df[i,]) 

