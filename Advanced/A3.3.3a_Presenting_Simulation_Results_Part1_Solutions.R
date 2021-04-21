#  Decision Modelling for Health Economic Evaluation
#  Advanced Course Exercise 3a (Part 1): SOLUTION FILE
#  Authors: Andrew Briggs, Jack Williams & Nichola Naylor

### Loading useful packages
library(data.table)
library(tidyr)
library(dplyr)
library(ggplot2)
library(reshape2) 

#  Reading the data needed from csv files
hazards <- read.csv("hazardfunction.csv", header=TRUE) ## importing the hazard inputs from the regression analysis
cov.55 <- read.csv("cov55.csv",row.names=1,header=TRUE) ## importing the covariance matrix
life.table <- read.csv("life-table.csv", header=TRUE)
life.table<- as.data.table(life.table)

####***** THR MODEL FUNCTION ****#####

#########**** PARAMETERS *****######

# SETTING CONSTANT PARAMETERS OUTSIDE THE FUNCTION 
##### DETERMINISTIC PARAMETERS ######
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
ab.uSuccessP <- mn.uSuccessP*(1-mn.uSuccessP)/(se.uSuccessP^2) ## estimating alpha plus beta (ab)
a.uSuccessP<-mn.uSuccessP*ab.uSuccessP ## estimating alpha (a)
b.uSuccessP<-a.uSuccessP*(1-mn.uSuccessP)/mn.uSuccessP ## estimating beta (b)

mn.uSuccessR<-0.75 ## mean utility value for having a successful Revision THR
se.uSuccessR<-0.04 ## standard error utility value for having a successful Revision THR
ab.uSuccessR<-mn.uSuccessR*(1-mn.uSuccessR)/(se.uSuccessR^2) ## alpha + beta (ab)
a.uSuccessR<-mn.uSuccessR*ab.uSuccessR ## alpha (a)
b.uSuccessR<-a.uSuccessR*(1-mn.uSuccessR)/mn.uSuccessR ## beta(b)

mn.uRevision<-0.30 ## mean utility score during the revision period
se.uRevision<-0.03 ## standard error utility score during the revision period
ab.uRevision<-mn.uRevision*(1-mn.uRevision)/(se.uRevision^2) ## alpha + beta (ab)
a.uRevision<-mn.uRevision*ab.uRevision ## alpha (a)
b.uRevision<-a.uRevision*(1-mn.uRevision)/mn.uRevision ## beta(b)

## discount matrices
cycle.v <- 1:cycles ## a vector of cycle numbers 1 - 60
discount.factor.c <- 1/(1+dr.c)^cycle.v ## the discount factor matrix
discount.factor.o <- 1/(1+dr.o)^cycle.v  ## discount factor matrix for utility 

model.THR <- function(age=60, male=0) {
  ### A function running the THR model, setting age and sex
  ### INPUTS: age = a numeric value, male = 0 for female and 1 for male 
  ### OUTPUTS: A numeric, named vector with variable labels of length = 6 variables
  
  ## LIFE TABLE DATA 
  # This is included within the function as it varies by age and sex (which are inputs into the function)
  colnames(life.table) <- c("Age","Index","Males","Female") ## making sure column names are correct
  current.age <- age + cycle.v ## a vector of cohort age throughout the model
  life.table <- as.data.table(life.table) ## turning life.table into a data.table 
  death.risk <- as.data.table(current.age) ## turning current age into a data.table 
  setkey(life.table,"Index") ## using the setkey function (read about it by typing in ?setkey in the console)
  setkey(death.risk,"current.age") ## using the setkey function for death.risk to sort and set current.age as the key
  death.risk <- life.table[death.risk, roll=TRUE] ## joining life.table and death.risk by the key columns, rolling forward between index values
  
  ###  Transition probabilities
  tp.PTHR2dead <- rbeta(1,a.PTHR2dead,b.PTHR2dead) ## Operative mortality rate  (OMR) following primary THR
  # since we assume the same shape parameters for RTHR : 
  tp.RTHR2dead <- rbeta(1,a.PTHR2dead,b.PTHR2dead)  ## Operative mortality rate (OMR) following revision THR
  
  tp.rrr <-rbeta(1,a.rrr,b.rrr) ## Re-revision risk 
  
  ##  Costs
  c.revision <- rgamma(1,shape=a.cRevision,scale=b.cRevision) ## Gamma distribution draw for cost of revision surgery
  state.costs<-c(c.primary, c.success, c.revision,c.success,0) ## a vector with the costs for each state
  
  ##  Utilities
  # primary prosthesis
  uSuccessP<-rbeta(1,a.uSuccessP,b.uSuccessP) ## drawing from the Beta distribution based on a and b
  ## revision surgery
  uSuccessR<-rbeta(1,a.uSuccessR,b.uSuccessR) ## drawing from the Beta distribution based on a and b
  uRevision<-rbeta(1,a.uRevision,b.uRevision) ## drawing from the Beta distribution based on a and b
  
  state.utilities<-c(0,uSuccessP,uRevision,uSuccessR,0)
  
  ##  Hazard function ####
  z<-rnorm(5,0,1) ## 5 random draws from the normal distribution
  Tz<-cholm%*%z ## Tz which is the Cholesky matrix multiplied by the 5 random draws
  x <- mn+Tz ## mu plus Tz
  r.lngamma<-x[1,1]  ## lngamma coefficient for the baseline hazard,based off of x
  r.cons<-x[2,1]   ## Constant in survival analysis for baseline hazard, based on x
  r.ageC<-x[3,1]  ## Age coefficient in survival analysis for baseline hazard based on x
  r.maleC<-x[4,1] 
  r.NP1<-x[5,1]
  
  gamma <- exp(r.lngamma) ## Ancilliary parameter in Weibull distribution (exponential of lngamma coefficient)
  lambda <- exp(r.cons+age*r.ageC+male*r.maleC) ## Lambda parameter survival analysis (depends on mix of above coefficients)
  RR.NP1 <- exp(r.NP1) ## Relative risk of revision for new prosthesis 1 compared to standard
 
   #####MARKOV MODEL #####
  
  #### TRANS AND TRACE ######
  #  Now create a transition matrix for the standard prosthesis arm and NP1 arm
  #  We start with a three dimensional array in order to capture the time dependencies
  revision.risk.sp0 <- 1- exp(lambda * ((cycle.v-1) ^gamma-cycle.v ^gamma))
  revision.risk.np1 <- 1- exp(lambda * RR.NP1 * ((cycle.v-1) ^gamma-cycle.v ^gamma))
  
  revision.risk.sp0 ## the time dependent risk of revision for standard treatment
  revision.risk.np1 ## the time dependent risk of revision for NP1
  
  # combining risks into a time-dependent transition probability data.table
  tdtps <- data.table(death.risk, revision.risk.sp0, revision.risk.np1)
  
  ## creating an indicator which selects the death risk column depending on the sex the model is being run on
  col.key <- 4-male ## 4 indicates the 4th column of tdps (which is female risk of death)
  ## when male=1 (i.e. male selected as sex) this becomes the 3rd column (which is male risk of death)
  
  ## Create a vector with the mortality values at each age, to insert into time dependent transition matrix below
  mortality.vec <- unname(unlist(death.risk[,..col.key]))
  
  #   STANDARD ARM
  #  Now create a transition matrix for the standard prosthesis arm
  #  We start with a three dimensional array in order to capture the time dependencies
  tm.SP0 <- array(data=0,dim=c(n.states, n.states, cycles),
                  dimnames= list(state.names, state.names, 1:cycles)) ## an empty array of dimenions (number of states, number of states, number of cycles)
  ## naming all dimensions
  
  ### create a loop that creates a time dependent transition matrix for each cycle
  for (i in 1:cycles) {
    
    ## tranisitions out of P-THR
    tm.SP0["P-THR","Death",i] <- tp.PTHR2dead ## Primary THR either enter the death state or.. or..
    tm.SP0["P-THR","successP-THR",i] <- 1 - tp.PTHR2dead ## they go into the success THR state 
    ## transitions out of success-P-THR
    tm.SP0["successP-THR","R-THR",i] <- revision.risk.sp0[i] ## could also link this to tdps if preferred
    tm.SP0["successP-THR","Death",i] <- mortality.vec[i]
    tm.SP0["successP-THR","successP-THR",i] <- 1-revision.risk.sp0[i] - mortality.vec[i]
    ## transitions out of R-THR 
    tm.SP0["R-THR","Death",i] <- tp.RTHR2dead + mortality.vec[i]
    tm.SP0["R-THR","successR-THR",i] <- 1 - tp.RTHR2dead - mortality.vec[i]
    ## transitions out of success-THR
    tm.SP0["successR-THR","R-THR",i] <- tp.rrr
    tm.SP0["successR-THR",5,i] <- mortality.vec[i]
    tm.SP0["successR-THR","successR-THR",i] <- 1 - tp.rrr - mortality.vec[i]
    
    tm.SP0["Death","Death",i] <- 1 ## no transitions out of death
  }
  
  tm.SP0
  
  #  Create a trace for the standard prosthesis arm
  trace.SP0 <- matrix(data=0, nrow=cycles, ncol=n.states)
  colnames(trace.SP0) <- state.names
  
  trace.SP0[1,] <- seed%*%tm.SP0[,,1]
  
  for (i in 2:cycles) {
    trace.SP0[i,] <- trace.SP0[i-1,]%*%tm.SP0[,,i]
  }
  trace.SP0
  
  rowSums(trace.SP0)
  
  #  NP1 ARM
  tm.NP1 <- array(data=0,dim=c(n.states, n.states, cycles),
                  dimnames= list(state.names, state.names, 1:cycles)) ## an empty array of dimenions (number of states, number of states, number of cycles)
  ## naming all dimensions
  
  ### create a loop that creates a time dependent transition matrix for each cycle
  for (i in 1:cycles) {
    
    ## tranisitions out of P-THR
    tm.NP1["P-THR","Death",i] <- tp.PTHR2dead ## Primary THR either enter the death state or.. or..
    tm.NP1["P-THR","successP-THR",i] <- 1 - tp.PTHR2dead ## they go into the success THR state 
    ## transitions out of success-P-THR
    tm.NP1["successP-THR","R-THR",i] <- revision.risk.np1[i] ## revision risk with NP1 treatment arm 
    tm.NP1["successP-THR","Death",i] <- mortality.vec[i]
    tm.NP1["successP-THR","successP-THR",i] <- 1 - revision.risk.np1[i] - mortality.vec[i]
    ## transitions out of R-THR 
    tm.NP1["R-THR","Death",i] <- tp.RTHR2dead + mortality.vec[i]
    tm.NP1["R-THR","successR-THR",i] <- 1 - tp.RTHR2dead - mortality.vec[i]
    ## transitions out of success-THR
    tm.NP1["successR-THR","R-THR",i] <- tp.rrr
    tm.NP1["successR-THR",5,i] <- mortality.vec[i]
    tm.NP1["successR-THR","successR-THR",i] <- 1 - tp.rrr - mortality.vec[i]
    
    tm.NP1["Death","Death",i] <- 1 ## no transitions out of death
  }
  
  tm.NP1
  
  #  Create a trace for the standard prosthesis arm
  trace.NP1 <- matrix(data=0,nrow=cycles,ncol=n.states)
  colnames(trace.NP1) <- state.names
  
  trace.NP1[1,] <- seed%*%tm.NP1[,,1]
  
  for (i in 2:cycles) {
    trace.NP1[i,] <- trace.NP1[i-1,]%*%tm.NP1[,,i]
  }
  trace.NP1
  
  rowSums(trace.NP1)
  
  #### COST #####
  # STANDARD ARM
  cost.SP0 <- trace.SP0%*%state.costs  ## the cost of SP0 based on cost per state and numbers in each state per cycle 
  # the above retruns a matrix of 1 column and 60 rows
  undisc.cost.SP0 <- colSums(cost.SP0) + c.SP0  ## the (undiscouted) sum of cost.SP0 plus the 1 off Cost of a primary THR procedure (c.SP0)
  
  disc.cost.SP0 <- (discount.factor.c%*%cost.SP0) + c.SP0   ## the discouted sum of cost.SP0 plus the 1 off Cost of a primary THR procedure (c.SP0)
  
  # NP1 ARM
  cost.NP1 <- trace.NP1%*%state.costs  ## the cost of NP1 based on cost per state and numbers in each state per cycle 
  undisc.cost.NP1 <- colSums(cost.NP1) + c.NP1  ## the (undiscouted) sum of cost.NP1 plus the 1 off Cost of a primary THR procedure (c.NP1)
  disc.cost.NP1 <- (discount.factor.c%*%cost.NP1) + c.NP1  ## the discouted sum of cost.SP0 plus the 1 off Cost of a primary THR procedure (c.NP1)
  
  ###QALYS######
  # STANDARD ARM
  QALYs.SP0 <- trace.SP0%*%state.utilities ## utility per cycle
  undisc.QALYs.SP0 <- colSums(QALYs.SP0) ## total undiscounted utility 
  
  disc.QALYs.SP0 <- colSums(discount.factor.o%*%QALYs.SP0) ## total discounted utility
  
  # NP1 ARM
  QALYs.NP1 <- trace.NP1%*%state.utilities ## utility per cycle
  undisc.QALYs.NP1 <- colSums(QALYs.NP1) ## total undiscounted utility 
  disc.QALYs.NP1 <- colSums(discount.factor.o%*%QALYs.NP1) ## total discounted utility
  
  undisc.QALYs.NP1 <- colSums(QALYs.NP1)
  disc.QALYs.NP1 <- discount.factor.o%*%QALYs.NP1

  ####ANALYSIS####
  output <- c(cost.SP0 = disc.cost.SP0,
              qalys.SP0 = disc.QALYs.SP0,
              cost.NP1 = disc.cost.NP1,
              qalys.NP1 = disc.QALYs.NP1,
              inc.cost = disc.cost.NP1 - disc.cost.SP0,
              inc.qalys = disc.QALYs.NP1 - disc.QALYs.SP0)
  
  return(output)

  
}

## testing the function:
model.THR(age=60, male=0) 


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

## have a look at what you've created so far:
head(sim.runs)

#### PLOTTING THE COST-EFFECTIVENESS PLANE #####

# simple base plot of incremental QALYs and costs
plot(simulation.results$inc.qalys,simulation.results$inc.cost)

## using pre-created ggplot2 functions for nicer cost-effectiveness plane graphs
source("ggplot_CEA_functions.R")
ce.plane(simulation.results)

## Estimating average ICER from the simulation
PSA.inc.cost <- mean(simulation.results$cost.NP1)-mean(simulation.results$cost.SP0)
PSA.inc.qalys <- mean(simulation.results$qalys.NP1)-mean(simulation.results$qalys.SP0)
PSA.icer <- PSA.inc.cost/PSA.inc.qalys

#### PLOTTING THE COST-EFFECTIVENESS ACCEPTABILITY CURVE #####

# Estimating the average net monetary benefit of the new treatment
WTP <- 100000 ## the ceiling ratio / willingness-to-pay
av.nmb <- PSA.inc.qalys*WTP - PSA.inc.cost  ## average net monetary benefit based on mean incremental costs and qalys from the simulation

# Estimate the probability of cost-effectiveness for a given willingness-to-pay ceiling ratio

p.CE<-function(WTP, simulation.results) {
## a function that estimates the probability of the new intervention
# being cost-effective 
# INPUTS: WTP = willingness to pay value (numeric)
#         simulation.results = a data.frame output of PSA simulations which includes
#         columns names "inc.qalys" and "inc.cost"   
# OUTPUTS: A numeric value specifying the probability of cost-effectiveness given the inputs
  
  nmb <- simulation.results[,"inc.qalys"]*WTP - simulation.results[,"inc.cost"] ## vector of NMB estimates for each simulation length 1:sim.runs
  CE <- nmb>0   ## vector of TRUE/FALSE for each simulation length 1:sim.runs
  probCE<- mean(CE) ## the mean value of TRUE (=1) and FALSE (=0)
   
  return(probCE)
  
}


# Generate CEAC table
WTP.values <- seq(from = 0, to = 50000, by = 10) ## use the seq() function to get a vector of specified numeric values
CEAC <- data.frame(WTP = WTP.values, 
                   pCE = rep(as.numeric(NA),length(WTP.values)))

for (i in 1:length(WTP.values)) {
  CEAC[i,"pCE"]<- p.CE(CEAC[i,"WTP"], simulation.results)
}

# Display the top and bottom of the CEAC table
head(CEAC)
tail(CEAC)


# Plotting the CEAC with a plot function
plot(CEAC$WTP,CEAC$pCE, type="l")

# Plotting the CEAC with the pre-defined ggplot functions
plot.ceac(CEAC)

##### SUBGROUP ANALYSES ######
# CREATE ARRAY TO STORE THE RESULTS OF THE MODEL IN EACH SUBGROUP

subgroups.names <- c("Male 40", "Male 60", "Male 80", "Female 40", "Female 60", "Female 80")
subgroups.n <- length(subgroups.names)

simulation.subgroups <- array(data = 0, dim = c(sim.runs, length(colnames(simulation.results)), subgroups.n),
                              dimnames = list(1:sim.runs, colnames(simulation.results),subgroups.names))

# Run model for each subgroup, inputting the age and sex into the function, and record results within the array
for(i in 1:sim.runs){
  simulation.subgroups[i,,1] <- model.THR(age = 40, male = 1)
  simulation.subgroups[i,,2] <- model.THR(age = 60, male = 1)
  simulation.subgroups[i,,3] <- model.THR(age = 80, male = 1)
  simulation.subgroups[i,,4] <- model.THR(age = 40, male = 0)
  simulation.subgroups[i,,5] <- model.THR(age = 60, male = 0)
  simulation.subgroups[i,,6] <- model.THR(age = 80, male = 0)
}



# Create a CEAC table with lambda value sequence
WTP.values <- seq(from = 0, to = 50000, by = 50)

CEAC.subgroups <- data.frame(matrix(data= as.numeric(NA), nrow=length(WTP.values), ncol=subgroups.n + 1))
colnames(CEAC.subgroups) <- c("WTP", subgroups.names)


# Estimate probability cost-effective for all subgroups
for (i in 1:length(WTP.values)) {

  CEAC.subgroups[i,1]<-WTP.values[i]
  CEAC.subgroups[i,2]<-p.CE(WTP.values[i], simulation.subgroups[,,1])
  CEAC.subgroups[i,3]<-p.CE(WTP.values[i], simulation.subgroups[,,2])
  CEAC.subgroups[i,4]<-p.CE(WTP.values[i], simulation.subgroups[,,3])
  CEAC.subgroups[i,5]<-p.CE(WTP.values[i], simulation.subgroups[,,4])
  CEAC.subgroups[i,6]<-p.CE(WTP.values[i], simulation.subgroups[,,5])
  CEAC.subgroups[i,7]<-p.CE(WTP.values[i], simulation.subgroups[,,6])

}

# Show the structure of the subgroup results 
head(CEAC.subgroups)

## Base R plot (col indicates colour, lty indicates linetype, where 1 = fill and 2 = dasshed) 

plot(CEAC.subgroups$WTP, CEAC.subgroups$`Male 40`, type="l", ylim = c(0,1), col = "red", lty = 2)
  lines(CEAC.subgroups$WTP, CEAC.subgroups$`Male 60`, col = "blue", lty = 2)
  lines(CEAC.subgroups$WTP, CEAC.subgroups$`Male 80`, col = "green", lty = 2)
  lines(CEAC.subgroups$WTP, CEAC.subgroups$`Female 40`, col = "red", lty = 1)
  lines(CEAC.subgroups$WTP, CEAC.subgroups$`Female 60`, col = "blue", lty = 1)
  lines(CEAC.subgroups$WTP, CEAC.subgroups$`Female 80`, col = "green", lty = 1)

## Alternative ggplot function for CEAC  
    
## We need to reshape the data from wide to long to use in ggplot 
CEAC.subgroups.long <- melt(CEAC.subgroups, id.vars = c("WTP"))
colnames(CEAC.subgroups.long) <- c("WTP", "subgroup", "pCE")
head(CEAC.subgroups.long)

# Plots of results using pre-defined ggplot functions
plot.ceac.all(CEAC.subgroups.long)

