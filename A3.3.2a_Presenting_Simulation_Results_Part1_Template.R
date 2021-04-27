#  Decision Modelling for Health Economic Evaluation
#  Course Exercise 3a (Part 1): TEMPLATE FILE
#  Authors: Andrew Briggs, Jack Williams & Nichola Naylor

### Loading useful packages
library(data.table)
library(tidyr)
library(dplyr)
library(ggplot2)
library(reshape2) 

###  Reading in the data needed from csv files
hazards <- read.csv("hazardfunction.csv", header=TRUE) ## importing the hazard inputs from the regression analysis
cov.55 <- read.csv("cov55.csv",row.names=1,header=TRUE) ## importing the covariance matrix
life.table <- read.csv("life-table.csv", header=TRUE)
life.table<- as.data.table(life.table)


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

### alpha and beta values :
  
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

# Discount factor matrices
cycle.v <- 1:cycles ## a vector of cycle numbers 1 - 60

discount.factor.c <-     ## the discount factor matrix for costs

discount.factor.o <-       ## discount factor matrix for utility 


model.THR <- function(age=60, male=0) {
  ### A function running the THR model, setting age and sex
  ### INPUTS: age = a numeric value, male = 0 for female and 1 for male 
  ### OUTPUTS: A numeric, named vector with variable labels of length = 6 variables
  
  ## LIFE TABLE DATA 
  # This is included within the function as it varies by age and sex (which are inputs into the function)
  colnames(life.table) <- c("Age","Index","Males","Female") ## making sure column names are correct
  cycle.v <-          ## a vector of cycle numbers 1 - number of cycles
  current.age <-      ## a vector of cohort age throughout the model
  
  life.table <- as.data.table(life.table) ## turning life.table into a data.table 
  death.risk <- as.data.table(current.age) ## turning current age into a data.table 
  setkey(life.table,"Index") ## using the setkey function (read about it by typing in ?setkey in the console)
  setkey(death.risk,"current.age") ## using the setkey function for death.risk to sort and set current.age as the key
  death.risk <- life.table[death.risk, roll=TRUE] ## joining life.table and death.risk by the key columns, rolling forward between index values
  
  ###  Transition probabilities
  tp.PTHR2dead <-       ## Operative mortality rate  (OMR) following primary THR
  # Note we assume the same shape parameters for RTHR : 
  tp.RTHR2dead <-       ## Operative mortality rate (OMR) following revision THR
  
  tp.rrr <-      ## Re-revision risk 
  
  ##  Costs
  c.revision <-        ## Gamma distribution draw for cost of revision surgery
 
  state.costs<-       ## a vector with the costs for each state
  
  ##  Utilities
  # primary prosthesis
  uSuccessP<-                  ## drawing from the Beta distribution based on a and b
  ## revision surgery
  uSuccessR<-                  ## drawing from the Beta distribution based on a and b
  uRevision<-                  ## drawing from the Beta distribution based on a and b
  
  state.utilities<-     ## a vector with the utilities for each state
  
  ##  Hazard function ####
  z <-  ## 5 random draws from the normal distribution
  Tz <-    ## Tz which is the Cholesky matrix multiplied by the 5 random draws
  x <-    ## mu plus Tz
  r.lngamma <-  ## lngamma coefficient for the baseline hazard,based off of x
  r.cons <-     ## Constant in survival analysis for baseline hazard based on x
  r.ageC <-    ## Age coefficient in survival analysis for baseline hazard based on x
  r.maleC <-    ## Male coefficient in survival analysis for baseline hazard based on x
  r.NP1 <-     ## NP1 coefficient for the baseline hazard, based off of x 
  
  gamma <- ## Ancilliary parameter in Weibull distribution (exponential of lngamma coefficient)
  lambda <-    ## Lambda parameter survival analysis (depends on mix of above coefficients)
  RR.NP1 <-  ## Relative risk of revision for new prosthesis 1 compared to standard
  
  #####MARKOV MODEL #####
  
  #### TRANS AND TRACE ######
  #  Now create a transition matrix for the standard prosthesis arm and NP1 arm
  #  We start with a three dimensional array in order to capture the time dependencies
  revision.risk.sp0 <- 1- exp(lambda * ((cycle.v-1) ^gamma-cycle.v ^gamma))## the time dependent risk of revision for standard treatment
  revision.risk.np1 <- 1- exp(lambda * RR.NP1 * ((cycle.v-1) ^gamma-cycle.v ^gamma)) ## the time dependent risk of revision for NP1
  
  # combining risks into a time-dependent transition probability data.table
  tdtps <-   ## use data.table()
  
  ## creating an indicator which selects the death risk column depending on the sex the model is being run on
  col.key <- 4-male ## 4 indicates the 4th column of tdps (which is female risk of death)
  ## when male=1 (i.e. male selected as sex) this becomes the 3rd column (which is male risk of death)
  
  ## Create a vector with the mortality values at each age, to insert into time dependent transition matrix below
  mortality.vec <- unname(unlist(death.risk[,..col.key]))
  
  #   STANDARD ARM
  #  Now create a transition matrix for the standard prosthesis arm
  #  We start with a three dimensional array in order to capture the time dependencies
  #  Create an empty array of dimenions (length; number of states, number of states, number of cycles):
  #  Don't forget to specify dimension names 
  tm.SP0 <-  ## use array()
    
  ### create a loop that creates a time dependent transition matrix for each cycle
  for (i in 1:cycles) {

    ## define the tm.SP0 values, using calculated transition probablilites (e.g. tp.rrr)
    ## and mortality.vec
    ## remember you can call columns/rows by name e.g.:
    ## tm.SP0["successR-THR","R-THR",i] <- tp.rrr
  }
  
  #  Create a trace for the standard prosthesis arm
  trace.SP0 <-   ## create a numeric, empty matrix with nrow= number of cycles, and ncol= number of states, use matrix()
  colnames(trace.SP0) <- state.names
  
  trace.SP0[1,] <-    ## calculating the transitions for cycle 1 seed * the relevant transition matrix
  
  for (i in 2:cycles) {
    trace.SP0[i,] <-  ## calculating the transitions for other cycles, trace cycle t-1 * the relevant transition matrix
  }
  
  #  NP1 ARM
  #  Create an empty array of dimenions (length; number of states, number of states, number of cycles):
  #  Don't forget to specify dimension names 
  tm.NP1 <-  ## use array()

  
  ### create a loop that creates a time dependent transition matrix for each cycle
  for (i in 1:cycles) {
    ## define the tm.SP0 values, using calculated transition probablilites (e.g. tp.rrr)
    ## and mortality.vec
    ## remember you can call columns/rows by name e.g.:
    ## tm.NP1["successR-THR","R-THR",i] <- tp.rrr
    ## tranisitions out of P-THR

  }
  
  #  Create a trace for the standard prosthesis arm
  trace.NP1 <-  ## create a numeric, empty matrix with nrow= number of cycles, and ncol= number of states
  colnames(trace.NP1) <- state.names
  
  trace.NP1[1,] <-  ## calculating the transitions for cycle 1 seed * the relevant transition matrix
  
  for (i in 2:cycles) {
    trace.NP1[i,] <-  ## calculating the transitions for other cycles, trace cycle t-1 * the relevant transition matrix
  }

  
  # rowSums(trace.SP0)
  # rowSums(trace.NP1)
  
  #### COST #####
  
  # STANDARD ARM
  cost.SP0 <- trace.SP0%*%state.costs  ## the cost of SP0 based on cost per state and numbers in each state per cycle 
  # the above retruns a matrix of 1 column and 60 rows
  undisc.cost.SP0 <-        ## the (undiscouted) sum of cost.SP0 plus the 1 off Cost of a primary THR procedure (c.SP0)
 
  disc.cost.SP0 <-        ## the discouted sum of cost.SP0 plus the 1 off Cost of a primary THR procedure (c.SP0)
  
  # NP1 ARM
  cost.NP1 <-       ## the cost of NP1 based on cost per state and numbers in each state per cycle 
  undisc.cost.NP1 <-      ## the (undiscouted) sum of cost.NP1 plus the 1 off Cost of a primary THR procedure (c.NP1)
  disc.cost.NP1 <-     ## the discouted sum of cost.SP0 plus the 1 off Cost of a primary THR procedure (c.NP1)
  
  ###QALYS######
  # STANDARD ARM
  QALYs.SP0 <-            ## utility per cycle
  undisc.QALYs.SP0 <-        ## total undiscounted utility 
    
  disc.QALYs.SP0 <-          ## total discounted utility
  
  # NP1 ARM
  QALYs.NP1 <-                  ## utility per cycle
  undisc.QALYs.NP1 <-          # total undiscounted utility 
  disc.QALYs.NP1 <-             ## total discounted utility
  
  ####ANALYSIS####
  output <- c(cost.SP0 =    , ## fill in with variable(s) listed above 
              qalys.SP0 =    ,
              cost.NP1 =     ,
              qalys.NP1 =    ,
              inc.cost =     , 
              inc.qalys =    )
  
  return(output)
  
}

## testing the function:
model.THR(age=60, male=0) 

#### RUNNING THE SIMULATION ####
sim.runs <-    ## the number of simulation runs

## creating an empty data.frame for simulation results to fill:
simulation.results <- data.frame("cost.SP0" =     , ## use the rep() function to create sim.runs rows of values
                                 "qalys.SP0"=     ,
                                 "cost.NP1" =     ,
                                 "qalys.NP1" =    ,
                                 "inc.cost" =     ,
                                 "inc.qalys"=     )

## running the simulations and filling the simulation.results data.frame:
for(i in 1:sim.runs){
  ## define each row as a result from each run  
  
}

## have a look at what you've created so far:
head(simulation.results)

#### PLOTTING THE COST-EFFECTIVENESS PLANE #####

# simple base plot of incremental QALYs and costs
plot(simulation.results$inc.qalys,simulation.results$inc.cost)

## using pre-created ggplot2 functions for nicer cost-effectiveness plane graphs
source("ggplot_CEA_functions.R")
ce.plane(simulation.results) 

## Estimating average ICER from the simulation:
# note you don't have to do this all in one calculation/variable definition

PSA.icer <- 

#### PLOTTING THE COST-EFFECTIVENESS ACCEPTABILITY CURVE #####

# Estimating the average net monetary benefit of the new treatment
WTP <-    ## the ceiling ratio / willingness-to-pay
av.nmb <-   ## average net monetary benefit based on mean incremental costs and qalys from the simulation

# Estimate the probability of cost-effectiveness for a given willingness-to-pay ceiling ratio

p.CE<-function(WTP, simulation.results) {
  ## a function that estimates the probability of the new intervention
  # being cost-effective 
  # INPUTS: WTP = willingness to pay value (numeric)
  #         simulation.results = a data.frame output of PSA simulations which includes
  #         columns names "inc.qalys" and "inc.cost"   
  # OUTPUTS: A numeric value specifying the probability of cost-effectiveness given the inputs
  
  ## write the function here that returns the probability of cost-effectiveness vector
  
  return(probCE)
  
}


# Generate CEAC table
WTP.values <-   ## use the seq() function to get a vector of numeric values in the specified range and increments
CEAC <- data.frame(WTP = WTP.values, 
                   pCE =   ) ## for pCE; use the rep() function to fill with missing numeric values

for (i in 1:length(WTP.values)) {
  CEAC[i,"pCE"]<-  ## use the p.CE function created above
}

# Display the top and bottom of the CEAC table
head(CEAC)
tail(CEAC)


# Plotting the CEAC with a plot function
plot() ## fill in accordingly 

# Plotting the CEAC with the pre-defined ggplot functions
plot.ceac(CEAC)

##### SUBGROUP ANALYSES ######

# CREATE ARRAY TO STORE THE RESULTS OF THE MODEL IN EACH SUBGROUP
subgroups.names <- c("Male 40", "Male 60", "Male 80", "Female 40", "Female 60", "Female 80")
subgroups.n <- length(subgroups.names)

simulation.subgroups <-  ## use array() and specifiy dimensions and dimension names using dimnames=

# Run model for each subgroup, inputting the age and sex into the function, and record results within the array
for(i in 1:sim.runs){
  simulation.subgroups[i,,1] <- model.THR(age = 40, male = 1)
  ### do the same for the other subgroups here
  
}

# Create a CEAC table with lambda value sequence
WTP.values <- seq(from = 0, to = 50000, by = 50)

CEAC.subgroups <- matrix(data= as.numeric(NA), nrow=length(WTP.values), ncol=subgroups.n + 1)
CEAC.subgroups <- as.data.frame(CEAC.subgroups)
colnames(CEAC.subgroups) <- c("WTP", subgroups.names)


# Estimate probability cost-effective for all subgroups
for (i in 1:length(WTP.values)) {
  
  CEAC.subgroups[i,1] <- WTP.values[i]
  ### using the p.CE function define the reat of CEAC.subgroups values for i here
  
  
}

# Show the structure of the subgroup results 
head(CEAC.subgroups)

## Base R plot (col indicates colour, lty indicates linetype, where 1 = fill and 2 = dasshed) 

plot(CEAC.subgroups$WTP, CEAC.subgroups$`Male 40`, type="l", ylim = c(0,1), col = "red", lty = 2)
lines(CEAC.subgroups$WTP, CEAC.subgroups$`Male 60`, col = "blue", lty = 2)
lines(CEAC.subgroups$WTP, CEAC.subgroups$`Male 80`, col = "green", lty = 2)
lines(CEAC.subgroups$WTP, CEAC.subgroups$`Female 40`, col = "red", lty = 1)
lines(CEAC.subgroups$WTP, CEAC.subgroups$`Female 60`, col = "blue", lty = 1)
lines(CEAC.subgroups$WTP, CEAC.subgroups$`Female 80`, col = "green", lty = 1) ## use the lines() function with plot() to generate 

## Alternative ggplot function for CEAC  

## We need to reshape the data from wide to long to use in ggplot 
CEAC.subgroups.long <-   ## use the melt function 
colnames(CEAC.subgroups.long) <- c("WTP", "subgroup", "pCE")

head(CEAC.subgroups.long) ## have a check of your long-format data

# Plots of results using pre-defined ggplot functions
plot.ceac.all(CEAC.subgroups.long)

