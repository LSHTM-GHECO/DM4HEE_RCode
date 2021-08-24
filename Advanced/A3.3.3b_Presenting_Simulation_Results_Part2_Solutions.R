#  Decision Modelling for Health Economic Evaluation
#  Advanced Course Exercise 3b (Part 2): SOLUTION FILE
#  Authors: Andrew Briggs, Jack Williams & Nichola Naylor

### Loading useful packages
library(ggplot2)
library(reshape2) 

#  Reading the data needed from csv files
hazards <- read.csv("hazardfunction_NP2.csv", header=TRUE) ## importing the hazard inputs from the regression analysis
cov.55 <- read.csv("cov55_NP2.csv",row.names=1,header=TRUE) ## importing the covariance matrix
life.table <- read.csv("life-table.csv", header=TRUE)


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
c.NP2 <- 788 ## Cost of new prosthesis 2

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
mn.NP2 <- hazards$coefficient[6] # new added comparator  

mn <- c(mn.lngamma, mn.cons,mn.ageC,mn.maleC,mn.NP1,mn.NP2) ## updated vector of mean values from the regression analysis

cholm <- t(chol(t(cov.55))) ## updated lower triangle of the Cholesky decomposition

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


model.THR.3b <- function(age=60, male=0) {
  ### A function running the THR model with an additional NP2 treatment pathway, setting age and sex
  ### INPUTS: age = a numeric value, male = 0 for female and 1 for male 
  ### OUTPUTS: A numeric, named vector with variable labels of length = 6 variables
  
  ## LIFE TABLE DATA 
  # This is included within the function as it varies by age and sex (which are inputs into the function)
  colnames(life.table) <- c("Age","Index","Males","Female") ## making sure column names are correct
  cycle.v <- 1:cycles ## a vector of cycle numbers 1 - 60
  current.age <- age + cycle.v ## a vector of cohort age throughout the model

  # This finds the position of age, within the life table 
  interval <- findInterval(current.age, life.table$Index)
  # These positions can then be used to subset the appropriate values from life.table
  death.risk <- data.frame(age = current.age, 
                           males = life.table[interval,3],
                           females = life.table[interval,4])
  
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
 
   ##  Hazard function
  z<-rnorm(6,0,1) ## 6 random draws from the normal distribution
  x<-mn + cholm %*%z
  Tz<-cholm%*%z ## Tz which is the Cholesky matrix multiplied by the 5 random draws
  x <- mn+Tz ## mu plus Tz
  r.lngamma<-x[1,1]  ## lngamma coefficient for the baseline hazard,based off of x
  r.cons<-x[2,1]   ## Constant in survival analysis for baseline hazard, based on x
  r.ageC<-x[3,1]  ## Age coefficient in survival analysis for baseline hazard based on x
  r.maleC<-x[4,1] 
  r.NP1<-x[5,1]
  r.NP2<-x[6,1] ## the new treatment arm coefficient
  
  gamma <- exp(r.lngamma) ## Ancilliary parameter in Weibull distribution (exponential of lngamma coefficient)
  lambda <- exp(r.cons+age*r.ageC+male*r.maleC) ## Lambda parameter survival analysis (depends on mix of above coefficients)
  RR.NP1 <- exp(r.NP1) ## Relative risk of revision for new prosthesis 1 compared to standard
  RR.NP2 <- exp(r.NP2) ## Relative risk of revision for new prosthesis 2 compared to standard
  
  #####MARKOV MODEL #####
  
  #### TRANS AND TRACE ######
  #  Now create a transition matrix for the standard prosthesis arm and NP1 arm
  #  We start with a three dimensional array in order to capture the time dependencies
  revision.risk.sp0 <- 1- exp(lambda * ((cycle.v-1) ^gamma-cycle.v ^gamma))
  revision.risk.np1 <- 1- exp(lambda * RR.NP1 * ((cycle.v-1) ^gamma-cycle.v ^gamma))
  revision.risk.np2 <- 1- exp(lambda * RR.NP2 * ((cycle.v-1) ^gamma-cycle.v ^gamma))
    
  revision.risk.sp0 ## the time dependent risk of revision for standard treatment
  revision.risk.np1 ## the time dependent risk of revision for NP1
  revision.risk.np2 ## the time dependent risk of revision for NP2
  
  # combining risks into a time-dependent transition probability data.frame
  tdtps <- data.frame(death.risk, revision.risk.sp0, revision.risk.np1, revision.risk.np2)
  
  ## creating an indicator which selects the death risk column depending on the sex the model is being run on
  col.key <- 3-male ## 3 indicates the 3rd column of tdps (which is female risk of death)
  ## when male=1 (i.e. male selected as sex) this becomes the 2nd column (which is male risk of death)
  
  ## Create a vector with the mortality values at each age, to insert into time dependent transition matrix below
  mortality.vec <- death.risk[,col.key]
  
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
  
  #  NP2 ARM
  tm.NP2 <- array(data=0,dim=c(n.states, n.states, cycles),
                  dimnames= list(state.names, state.names, 1:cycles)) ## an empty array of dimenions (number of states, number of states, number of cycles)
  ## naming all dimensions
  
  ### create a loop that creates a time dependent transition matrix for each cycle
  for (i in 1:cycles) {
    
    ## tranisitions out of P-THR
    tm.NP2["P-THR","Death",i] <- tp.PTHR2dead ## Primary THR either enter the death state or.. or..
    tm.NP2["P-THR","successP-THR",i] <- 1 - tp.PTHR2dead ## they go into the success THR state 
    ## transitions out of success-P-THR
    tm.NP2["successP-THR","R-THR",i] <- revision.risk.np2[i] ## revision risk with NP2 treatment arm 
    tm.NP2["successP-THR","Death",i] <- mortality.vec[i]
    tm.NP2["successP-THR","successP-THR",i] <- 1 - revision.risk.np2[i] - mortality.vec[i]
    ## transitions out of R-THR 
    tm.NP2["R-THR","Death",i] <- tp.RTHR2dead + mortality.vec[i]
    tm.NP2["R-THR","successR-THR",i] <- 1 - tp.RTHR2dead - mortality.vec[i]
    ## transitions out of success-THR
    tm.NP2["successR-THR","R-THR",i] <- tp.rrr
    tm.NP2["successR-THR",5,i] <- mortality.vec[i]
    tm.NP2["successR-THR","successR-THR",i] <- 1 - tp.rrr - mortality.vec[i]
    
    tm.NP2["Death","Death",i] <- 1 ## no transitions out of death
  }
  
  tm.NP2
  
  #  Create a trace for the standard prosthesis arm
  trace.NP2 <- matrix(data=0,nrow=cycles,ncol=n.states)
  colnames(trace.NP2) <- state.names
  
  trace.NP2[1,] <- seed%*%tm.NP2[,,1]
  
  for (i in 2:cycles) {
    trace.NP2[i,] <- trace.NP2[i-1,]%*%tm.NP2[,,i]
  }
  trace.NP2
  
  rowSums(trace.NP2)
  
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
  
  # NP2 ARM
  cost.NP2 <- trace.NP2%*%state.costs  ## the cost of NP2 based on cost per state and numbers in each state per cycle 
  undisc.cost.NP2 <- colSums(cost.NP2) + c.NP2  ## the (undiscouted) sum of cost.NP2 plus the 1 off Cost of a primary THR procedure (c.NP2)
  disc.cost.NP2 <- (discount.factor.c%*%cost.NP2) + c.NP2  ## the discouted sum of cost.SP0 plus the 1 off Cost of a primary THR procedure (c.NP2)
  
  
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
  
  # NP2 ARM
  QALYs.NP2 <- trace.NP2%*%state.utilities ## utility per cycle
  undisc.QALYs.NP2 <- colSums(QALYs.NP2) ## total undiscounted utility 
  disc.QALYs.NP2 <- colSums(discount.factor.o%*%QALYs.NP2) ## total discounted utility
  undisc.QALYs.NP2 <- colSums(QALYs.NP2)
  disc.QALYs.NP2 <- discount.factor.o%*%QALYs.NP2
  
  ####ANALYSIS####
  output <- c(cost.SP0 = disc.cost.SP0,
              qalys.SP0 = disc.QALYs.SP0,
              cost.NP1 = disc.cost.NP1,
              qalys.NP1 = disc.QALYs.NP1,
              cost.NP2 = disc.cost.NP2,
              qalys.NP2 = disc.QALYs.NP2)
  
  return(output)
  
  
}

## testing the function:
model.THR.3b(age=60, male=0) 

#### RUNNING THE SIMULATIONS ########
sim.runs <- 1000 ## the number of simulation runs

## creating an empty data.frame for simulation results to fill:
simulation.results <- data.frame("cost.SP0" = rep(as.numeric(NA), sim.runs), ## use the rep() function to create sim.runs rows of values
                                 "qalys.SP0"= rep(as.numeric(NA),sim.runs),
                                 "cost.NP1" = rep(as.numeric(NA),sim.runs),
                                 "qalys.NP1" = rep(as.numeric(NA), sim.runs),
                                 "cost.NP2" = rep(as.numeric(NA),sim.runs),
                                 "qalys.NP2"=  rep(as.numeric(NA),sim.runs))

## adding a progress bar to the simulatio runs allows you to keep track of where the simulation is
# this is particularly useful for more complex models that may take longer to run

pb = txtProgressBar(min = 0, max = sim.runs, initial = 0, style = 3)

for(i in 1:sim.runs) {
  setTxtProgressBar(pb,i)  
  simulation.results[i,] <- model.THR.3b(60,0)
}

## have a look at what you've created so far:
head(simulation.results)

# calculate the mean results across simulations
mean.results <- apply(simulation.results, 2, mean)
mean.results

#### PLOTTING THE COST-EFFECTIVENESS PLANE #####
## loading our predefined functions 
# feel free to take a look at the script and see the code itself
# just open up the "ggplot_CEA_functions.R" file
source("ggplot_CEA_functions.R")

## need to format the data to get into the correct format
# want costs and outcomes for each group for each run
standard.sims <- data.frame(comparator = "SP0", cost = simulation.results$cost.SP0, qaly = simulation.results$qalys.SP0)
NP1.sims <- data.frame(comparator = "NP1", cost = simulation.results$cost.NP1, qaly = simulation.results$qalys.NP1)
NP2.sims <- data.frame(comparator = "NP2", cost = simulation.results$cost.NP2, qaly = simulation.results$qalys.NP2)
plane.sims <- rbind(standard.sims, NP1.sims, NP2.sims)

## plotting this using predefined functions:
plot.ce.plane.all(plane.sims)

#### PLOTTING THE COST-EFFECTIVENESS ACCEPTABILITY CURVE #####

# Generate CEAC table
WTP.values <- seq(from = 0, to = 50000, by = 100) ## use the seq() function to get a vector of specified numeric values

CEAC <- data.frame(WTP = WTP.values, 
                   Standard= rep(as.numeric(NA),length(WTP.values)),
                   NP1= rep(as.numeric(NA),length(WTP.values)),
                   NP2= rep(as.numeric(NA),length(WTP.values)))

pCE.3b <-function(WTP, simulation.results) {
  ## a function that estimates the probability of the new intervention
  # being cost-effective 
  # INPUTS: WTP = willingness to pay value (numeric)
  #         simulation.results = a data.frame output of PSA simulations which includes
  #         columns prefixed with "cost." and "qalys." for SP0, NP1 and NP2
  # OUTPUTS: A numeric value specifying the probability of cost-effectiveness across the 3 pathways given the inputs
  
  # Now calculate NMB for all three interventions
  nmb <- simulation.results[,c("qalys.SP0",
                               "qalys.NP1",
                               "qalys.NP2")]*WTP - 
                                    simulation.results[,c("cost.SP0",
                                                          "cost.NP1",
                                                          "cost.NP2")] 
  ## this time we want to choose the prothesis with the greatest net monetary benefit:
  max.nmb <- apply(nmb, 1, max) # selecting max value indication by row within nmb
  
  ## creating an indication of TRUE/FALSE as to whether each treatment column == that max value:
  CE <- nmb[1:nrow(simulation.results),] == max.nmb[1:nrow(simulation.results)] 
  probCE<- apply(CE, 2, mean) ## averaging over TRUE (=1) and FALE (=0) for each column
  
  return(probCE)
  
}


pb = txtProgressBar(min = 0, max = length(WTP.values), initial = 0, style = 3)

for (i in 1:length(WTP.values)) {
  setTxtProgressBar(pb,i)
  CEAC[i,"WTP"] <- WTP.values[i]
  CEAC[i,2:4]<- pCE.3b(WTP.values[i], simulation.results)
  
}


# Look at the results
head(CEAC)  
tail(CEAC)

# Plotting the CEAC with a plot function
# reshape to show all in on plot
CEAC.long <- melt(CEAC, id.vars = c("WTP"))
colnames(CEAC.long) <- c("WTP", "group", "pCE")
head(CEAC.long)

plot.ceac.all(CEAC.long)



## Cost-effectiveness acceptability frontier (CEAF) - Additional code

# This code is very similar to the pCE.3b() code (that is used to produce the CEAC table)
# We have annoted the function to note where the differences are, including how the treatment with the 
#  maximum NMB is selected from the data

CEAF.function <-function(WTP, simulation.results) {

  # The main code here is the same as that used in the CEAC 
  nmb <- simulation.results[,c("qalys.SP0",
                               "qalys.NP1",
                               "qalys.NP2")]*WTP - 
    simulation.results[,c("cost.SP0",
                          "cost.NP1",
                          "cost.NP2")] 
  ## this time we want to choose the prothesis with the greatest net monetary benefit:
  colnames(nmb) <- c("SP0", "NP1", "NP2")
  
  # Here we calcualte the probability of cost-effectiveness, as per original function 
  max.nmb <- apply(nmb, 1, max) # selecting max value indication by row within nmb
  CE <- nmb[1:nrow(simulation.results),] == max.nmb[1:nrow(simulation.results)] 
  probCE<- apply(CE, 2, mean) ## averaging over TRUE (=1) and FALE (=0) for each column
  
  # However, we want the probability of cost-effectiveness for the treatment with the maximum overall NMB
  av.nmb <- colMeans(nmb)
  # This will check if the av.nmb is the highest value, and will subset the prob.CE results accordingly 
  # (i.e. where av.nmb is maximised, the probCE value will be returned)
  max.ce <- probCE[av.nmb == max(av.nmb)]
  # We take the name of the treatment so we can know which of the three treatments produced the highest NMB! 
  comparator <- names(max.ce) 
  
  return(data.frame(comparator, max.ce))
  
}

# Create an empty table for the results
CEAF <- data.frame(WTP = as.numeric(WTP.values), 
                   group = rep(NA,length(WTP.values)),
                   pCE = rep(as.numeric(NA),length(WTP.values)))


for(i in 1:length(WTP.values)){
  CEAF[i,2:3] <- CEAF.function(WTP.values[i], simulation.results)
}

# The same ggplot code can be used for the CEAF plot 
plot.ceac.all(CEAF)


