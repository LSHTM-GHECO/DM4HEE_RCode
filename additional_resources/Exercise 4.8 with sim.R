#  DM4HEE 
#  Exercise 4.8 - Making the THR model probabilistic
#  Author: Andrew Briggs
#  Date created: 22 February 2021
#  Date last edit: 12 March 2021

### Loading useful packages
library(data.table)
library(tidyr)
library(dplyr)

#  Reading the data needed from csv files
hazards <- read.csv("inputs/hazardfunction.csv", header=TRUE) ## importing the hazard inputs from the regression analysis

cov.55<-read.csv("inputs/cov55.csv",row.names=1,header=TRUE) ## importing the 

life.table <- read.csv("inputs/life-table.csv", header=TRUE)
life.table<- as.data.table(life.table)

#########**** PARAMETERS *****######
#  Start by defining parameters
##### DETERMINISTIC PARAMETERS ######
age <- 60 ## set age group for analyses
male <- 0 ## set sex identified, 0 = female and 1 = male
## the specific number (0,1) becomes important for reasons you'll see further down the script
dr.c <- 0.06 ## set the discount rate for costs (6%)
dr.o <- 0.015 ## set the discount rate for outcomes (15%)

cycles <- 60 ## number of cycles running the model

state.names <- c("P-THR","successP-THR","R-THR","successR-THR","Death")
n.states <- length(state.names)

c.SP0 <- 394 ## Cost of standard prosthesis
c.NP1 <- 579 ## Cost of new prosthesis 1

#  Seed the starting states of the model
seed <- c(1,0,0,0,0)


#### Model function ##### 

model.THR <- function() {
  
#### PROBABLISTIC PARAMETERS #####

###  Transition probabilities

a.PTHR2dead <- 2 ## alpha value for operative mortality from primary surgery
b.PTHR2dead <- 100- a.PTHR2dead ## beta value for operative mortality from primary surgery

tp.PTHR2dead <- rbeta(1,a.PTHR2dead,b.PTHR2dead) ## Operative mortality rate  (OMR) following primary THR
# since we assume the same shape parameters for RTHR : 
tp.RTHR2dead <- rbeta(1,a.PTHR2dead,b.PTHR2dead)  ## Operative mortality rate (OMR) following revision THR

a.rrr <- 4
b.rrr <- 100-a.rrr
tp.rrr <-rbeta(1,a.rrr,b.rrr) ## Re-revision risk 

tp.PTHR2dead
tp.RTHR2dead
tp.rrr

# calculating deterministic mean and SE as a comparison for these 3 transition probabilities 
mn.PTHR2dead <- a.PTHR2dead/(a.PTHR2dead+b.PTHR2dead) ## mean tp.PTHR2dead
se.PTHR2dead <- sqrt(a.PTHR2dead*b.PTHR2dead/((a.PTHR2dead+b.PTHR2dead)^2*
                                                (a.PTHR2dead+b.PTHR2dead+1))) ## standard error for tp.PTHR2dead
# the values are the same for revision surgery, as the alpha and beta are assumed to be the same
mn.PTHR2dead
se.PTHR2dead 

mn.rrr <- a.rrr/(a.rrr+b.rrr) ## mean value for Re-revision risk 
se.rrr <- sqrt(a.rrr*b.rrr/((a.rrr+b.rrr)^2*
                              (a.rrr+b.rrr+1))) ## standard error value for Re-revision risk 



##  Costs
c.primary <- 0  ## Cost of a primary THR procedure - 
## Note that the cost of the primary procedure is excluded (set to 0): since both arms have this procedure it is assumed to net out of the incremental analysis.  However, if the model was to be used to estimate lifetime costs of THR it would be important to include.
mn.cRevision <- 5294 ## mean cost of revision surgery
se.cRevision <- 1487 ## standard error of cost of revision surgery

a.cRevision <- (mn.cRevision/se.cRevision)^2 ## alpha value for cost of revision surgery 
b.cRevision <- (se.cRevision^2)/mn.cRevision ## beta value for cost of revision surgery
c.revision <- rgamma(1,shape=a.cRevision,scale=b.cRevision) ## Gamma distribution draw for cost of revision surgery

c.success <- 0 ## Cost of one cycle in a 'success' state (primary or revision)
## Note for c.sucess There are assumed to be no ongoing monitoring costs for successful THR.  However, this parameter is included in case users want to change this assumption.

## remember that c.SP0 and c.NP1 are fixed and were defined at the beginning of this exercise

state.costs<-c(c.primary, c.success, c.revision,c.success,0) ## a vector with the costs for each state


##  Utilities

# primary prosthesis
mn.uSuccessP <- 0.85 ## mean utility value for successful primary prosthesis
se.uSuccessP <- 0.03 ## standard errror utility value for successful primary prosthesis

ab.uSuccessP <- mn.uSuccessP*(1-mn.uSuccessP)/(se.uSuccessP^2) ## estimating alpha plus beta (ab)
a.uSuccessP<-mn.uSuccessP*ab.uSuccessP ## estimating alpha (a)
b.uSuccessP<-a.uSuccessP*(1-mn.uSuccessP)/mn.uSuccessP ## estimating beta (b)
uSuccessP<-rbeta(1,a.uSuccessP,b.uSuccessP) ## drawing from the Beta distribution based on a and b

## revision surgery
mn.uSuccessR<-0.75 ## mean utility value for having a successful Revision THR
se.uSuccessR<-0.04 ## standard error utility value for having a successful Revision THR

ab.uSuccessR<-mn.uSuccessR*(1-mn.uSuccessR)/(se.uSuccessR^2) ## alpha + beta (ab)
a.uSuccessR<-mn.uSuccessR*ab.uSuccessR ## alpha (a)
b.uSuccessR<-a.uSuccessR*(1-mn.uSuccessR)/mn.uSuccessR ## beta(b)
uSuccessR<-rbeta(1,a.uSuccessR,b.uSuccessR) ## drawing from the Beta distribution based on a and b

## during the revision period 
mn.uRevision<-0.30 ## mean utility score during the revision period
se.uRevision<-0.03 ## standard error utility score during the revision period

ab.uRevision<-mn.uRevision*(1-mn.uRevision)/(se.uRevision^2) ## alpha + beta (ab)
a.uRevision<-mn.uRevision*ab.uRevision ## alpha (a)
b.uRevision<-a.uRevision*(1-mn.uRevision)/mn.uRevision ## beta(b)
uRevision<-rbeta(1,a.uRevision,b.uRevision) ## drawing from the Beta distribution based on a and b

state.utilities<-c(0,uSuccessP,uRevision,uSuccessR,0)

##  Hazard function ####
## Coefficients - on the log hazard scale
mn.lngamma <- hazards$coefficient[1] ## Ancilliary parameter in Weibull distribution - equivalent to lngamma coefficient
mn.cons <- hazards$coefficient[2] ##Constant in survival analysis for baseline hazard
mn.ageC <- hazards$coefficient[3] ## Age coefficient in survival analysis for baseline hazard
mn.maleC <- hazards$coefficient[4] ## Male coefficient in survival analysis for baseline hazard
mn.NP1 <- hazards$coefficient[5]

mn<-c(mn.lngamma, mn.cons,mn.ageC,mn.maleC,mn.NP1) ## vector of mean values from the regression analysis

cholm <- t(chol(t(cov.55))) ## lower triangle of the Cholesky decomposition

z<-rnorm(5,0,1) ## 5 random draws from the normal distribution

Tz<-cholm%*%z ## Tz which is the Cholesky matrix multiplied by the 5 random draws

x <- mn+Tz ## mu plus Tz

r.lngamma<-x[1,1] 
r.cons<-x[2,1]
r.ageC<-x[3,1]
r.maleC<-x[4,1]
r.NP1<-x[5,1]

gamma <- exp(r.lngamma)
lambda <- exp(r.cons+age*r.ageC+male*r.maleC)
RR.NP1 <- exp(r.NP1)

# sp0.LP<-cons+age*ageC+male*maleC
# np1.LP<-cons+age*ageC+male*maleC+NP1
## trying to keep consistent with the exercise 3.5 and excel


##### LIFE TABLES #####

colnames(life.table) <- c("Age","Index","Males","Female") ## making sure column names are correct

cycle.v <- 1:cycles ## a vector of cycle numbers 1 - 60
current.age <- age + cycle.v ## a vector of cohort age throughout the model
current.age

## creating a table that has every age of the cohort plus death risks associated with that age
life.table <- as.data.table(life.table) ## turning life.table into a data.table 
death.risk <- as.data.table(current.age) ## turning current age into a data.table 
setkey(life.table,"Index") ## using the setkey function (read about it by typing in ?setkey in the console)
setkey(death.risk,"current.age") ## using the setkey function for death.risk to sort and set current.age as the key
death.risk <- life.table[death.risk, roll=TRUE] ## joining life.table and death.risk by the key columns, rolling forward between index values


#####***** MARKOV MODEL ****#####

#### TRANS AND TRACE ######
#  Now create a transition matrix for the standard prosthesis arm and NP1 arm
#  We start with a three dimensional array in order to capture the time dependencies
revision.risk.sp0 <- 1- exp(lambda * ((cycle.v-1) ^gamma-cycle.v ^gamma))
revision.risk.np1 <- 1- exp(lambda * RR.NP1 * ((cycle.v-1) ^gamma-cycle.v ^gamma))

revision.risk.sp0 ## the time dependent risk of revision for standard treatment
revision.risk.np1 ## the time dependent risk of revision for NP1

# combining risks into a time-dependent transition probability data.table
tdtps <- data.table(death.risk, revision.risk.sp0, revision.risk.np1)
tdtps

## creating an indicator which selects the death risk column depending on the sex the model is being run on
col.key <- 4-male ## 4 indicates the 4th column of tdps (which is female risk of death)
## when male=1 (i.e. male selected as sex) this becomes the 3rd column (which is male risk of death)

#   STANDARD ARM
#  Now create a transition matrix for the standard prosthesis arm
#  We start with a three dimensional array in order to capture the time dependencies
tm.SP0 <- array(data=0,dim=c(n.states, n.states, cycles),
                dimnames= list(state.names, state.names, 1:cycles)) ## an empty array of dimenions (number of states, number of states, number of cycles)
## naming all dimensions

### create a loop that creates a time dependent transition matrix for each cycle
for (i in 1:cycles) {
  
  mortality <- as.numeric(tdtps[i,..col.key]) 
  ## tranisitions out of P-THR
  tm.SP0["P-THR","Death",i] <- tp.PTHR2dead ## Primary THR either enter the death state or.. or..
  tm.SP0["P-THR","successP-THR",i] <- 1 - tp.PTHR2dead ## they go into the success THR state 
  ## transitions out of success-P-THR
  tm.SP0["successP-THR","R-THR",i] <- revision.risk.sp0[i]
  tm.SP0["successP-THR","Death",i] <- mortality
  tm.SP0["successP-THR","successP-THR",i] <- 1-revision.risk.sp0[i] - mortality
  ## transitions out of R-THR 
  tm.SP0["R-THR","Death",i] <- tp.RTHR2dead + mortality
  tm.SP0["R-THR","successR-THR",i] <- 1 - tp.RTHR2dead - mortality 
  ## transitions out of success-THR
  tm.SP0["successR-THR","R-THR",i] <- tp.rrr
  tm.SP0["successR-THR",5,i] <- mortality
  tm.SP0["successR-THR","successR-THR",i] <- 1 - tp.rrr - mortality
  
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
  
  mortality <- as.numeric(tdtps[i,..col.key]) 
  ## tranisitions out of P-THR
  tm.NP1["P-THR","Death",i] <- tp.PTHR2dead ## Primary THR either enter the death state or.. or..
  tm.NP1["P-THR","successP-THR",i] <- 1 - tp.PTHR2dead ## they go into the success THR state 
  ## transitions out of success-P-THR
  tm.NP1["successP-THR","R-THR",i] <- revision.risk.np1[i] ## revision risk with NP1 treatment arm 
  tm.NP1["successP-THR","Death",i] <- mortality
  tm.NP1["successP-THR","successP-THR",i] <- 1 - revision.risk.np1[i] - mortality
  ## transitions out of R-THR 
  tm.NP1["R-THR","Death",i] <- tp.RTHR2dead + mortality
  tm.NP1["R-THR","successR-THR",i] <- 1 - tp.RTHR2dead - mortality 
  ## transitions out of success-THR
  tm.NP1["successR-THR","R-THR",i] <- tp.rrr
  tm.NP1["successR-THR",5,i] <- mortality
  tm.NP1["successR-THR","successR-THR",i] <- 1 - tp.rrr - mortality
  
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

rowSums(trace.SP0)

#### COST #####

# STANDARD ARM
cost.SP0 <- trace.SP0%*%state.costs
cost.SP0

undisc.cost.SP0 <- colSums(cost.SP0) + c.SP0
undisc.cost.SP0

discount.factor.c <- 1/(1+dr.c)^cycle.v
discount.factor.c

disc.cost.SP0 <- (discount.factor.c%*%cost.SP0) + c.SP0
disc.cost.SP0

# NP1 ARM
cost.NP1 <- trace.NP1%*%state.costs
cost.NP1

undisc.cost.NP1 <- colSums(cost.NP1) + c.NP1
undisc.cost.NP1

disc.cost.NP1 <- (discount.factor.c%*%cost.NP1) + c.NP1
disc.cost.NP1

###QALYS######

# STANDARD ARM
QALYs.SP0 <- trace.SP0%*%state.utilities
QALYs.SP0

undisc.QALYs.SP0 <- colSums(QALYs.SP0)
undisc.QALYs.SP0

discount.factor.o <- 1/(1+dr.o)^cycle.v 
discount.factor.o

disc.QALYs.SP0 <- discount.factor.o%*%QALYs.SP0
disc.QALYs.SP0

# NP1 ARM
QALYs.NP1 <- trace.NP1%*%state.utilities
QALYs.NP1

undisc.QALYs.NP1 <- colSums(QALYs.NP1)
undisc.QALYs.NP1

disc.QALYs.NP1 <- discount.factor.o%*%QALYs.NP1
disc.QALYs.NP1

####****ANALYSIS****####
output <- c(inc.cost = disc.cost.NP1 - disc.cost.SP0,
            inc.lys = disc.QALYs.NP1 - disc.QALYs.SP0)

return(output)
}


# You can run the function to get the results of a simulation (each simulation will provide different results!)
model.THR()


#### Probabilistic model runs #### 

# Now we know we can generate model results using a function, we can run and store a number of simulations

# First, we set the number of simulations to run  
sim.runs <- 1000

# We can create a data frame to store results
simulation.results <- data.frame(matrix(0, nrow = sim.runs, ncol = 2))
colnames(simulation.results) <- c("inc.LYs","inc.costs")

#### Running simulations & recording results #### 

# Now Loop through the appropriate number of simulations and store the function results into the results data.frame 
for(i in 1:sim.runs) simulation.results[i,] <- model.THR() 


# We can check that the results are storing correctly 
head(simulation.results)



################### PREVIOUS MODEL CODE #################
### EDIT TOGETHER WITH ABOVE/REMOVE

# library(data.table)
# 
# #  Read in the life table and covariance table
# 
# life.table <- read.csv("inputs/life-table.csv")
# life.table<-data.table(life.table)
# cov.55<-read.csv("inputs/cov55.csv",row.names=1,header=TRUE) ## 1st col and row as labels
# # Define fixed parameters outside of the model to avoid repition
# 
# cDR<-0.06
# oDR<-0.015
# 
# #  Seed the starting states of the model
# seed<-c(1,0,0,0,0)
# 
# #  Set the total number of cycles to run and name the states
# cycles<-60
# 
# state.names<-c("P-THR","successP-THR","R-THR","successR-THR","Death")
# n.states<-length(state.names)
# 
# 
# # O.discount.factor<-matrix(data=NA,nrow=1,ncol=cycles)
# # for (i in 1:cycles) {
# #   O.discount.factor[1,i]<-1/(1+oDR)^i
# # }
# # O.discount.factor
# O.discount.factor <- matrix(1/(1+oDR) ^ c(1:cycles), nrow = 1, ncol = cycles)
# O.discount.factor
# 
# 
# # C.discount.factor<-matrix(data=NA,nrow=1,ncol=cycles)
# # for (i in 1:cycles) {
# #   C.discount.factor[1,i]<-1/(1+cDR)^i
# # }
# # C.discount.factor
# C.discount.factor <- matrix(1/(1+cDR) ^ c(1:cycles), nrow = 1, ncol = cycles)
# C.discount.factor
# 
#  model.THR<-function(age, male) {
# 
# #  Defining parameters
# 
# #  Demographics
# 
# #age<-60
# #male<-0
# 
# #  Transition probabilities
# 
# omrPTHR<-rbeta(1,2,98)
# omrRTHR<-rbeta(1,2,98)
# rrr<-rbeta(1,4,96)
# 
# #  Revision rates
# 
# mn.cons<--5.490935
# mn.ageC<--0.0367022
# mn.maleC<-0.768536
# mn.NP1<--1.344474
# mn.lngamma<-0.3740968
# mn<-c(mn.lngamma, mn.cons,mn.ageC,mn.maleC,mn.NP1)
# 
# cholm <- t(chol(t(cov.55)))
# z<-rnorm(5,0,1)
# x<-mn+cholm%*%z
# 
# lngamma<-x[1,1]
# cons<-x[2,1]
# ageC<-x[3,1]
# maleC<-x[4,1]
# NP1<-x[5,1]
# 
# sp0.LP<-cons+age*ageC+male*maleC
# np1.LP<-cons+age*ageC+male*maleC+NP1
# 
# #  Costs
# 
# cPrimary<-0
# mn.cRevision<-5294
# se.cRevision<-1487
# a.cRevision<-(mn.cRevision/se.cRevision)^2
# b.cRevision<-(se.cRevision^2)/mn.cRevision
# cRevision<-rgamma(1,shape=a.cRevision,scale=b.cRevision)
# cSuccess<-0
# cSP0<-394
# cNP1<-579
# state.costs<-c(cPrimary, cSuccess, cRevision,cSuccess,0)
# 
# #  Utilities
# 
# mn.uSuccessP<-0.85
# se.uSuccessP<-0.03
# ab.uSuccessP<-mn.uSuccessP*(1-mn.uSuccessP)/(se.uSuccessP^2)
# a.uSuccessP<-mn.uSuccessP*ab.uSuccessP
# b.uSuccessP<-a.uSuccessP*(1-mn.uSuccessP)/mn.uSuccessP
# uSuccessP<-rbeta(1,a.uSuccessP,b.uSuccessP)
# 
# mn.uSuccessR<-0.75
# se.uSuccessR<-0.04
# ab.uSuccessR<-mn.uSuccessR*(1-mn.uSuccessR)/(se.uSuccessR^2)
# a.uSuccessR<-mn.uSuccessR*ab.uSuccessR
# b.uSuccessR<-a.uSuccessR*(1-mn.uSuccessR)/mn.uSuccessR
# uSuccessR<-rbeta(1,a.uSuccessR,b.uSuccessR)
# 
# mn.uRevision<-0.30
# se.uRevision<-0.03
# ab.uRevision<-mn.uRevision*(1-mn.uRevision)/(se.uRevision^2)
# a.uRevision<-mn.uRevision*ab.uRevision
# b.uRevision<-a.uRevision*(1-mn.uRevision)/mn.uRevision
# uRevision<-rbeta(1,a.uRevision,b.uRevision)
# 
# state.utilities<-c(0,uSuccessP,uRevision,uSuccessR,0)
# 
# 
# 
# #  Create revision and death risk as function of age
# 
# mr<-3-male
# cycle<-1:cycles
# current.age<-age+cycle
# #current.age
# death.risk<-data.table(current.age)
# setkey(life.table,"Index")
# setkey(death.risk,"current.age")
# death.risk<-life.table[death.risk, roll=TRUE]
# death.risk.vector <- as.vector(as.matrix(death.risk[,..mr]))
# 
# revision.risk.sp0<-1-exp(exp(sp0.LP)*((cycle-1)^exp(lngamma)-cycle^exp(lngamma)))
# revision.risk.np1<-1-exp(exp(np1.LP)*((cycle-1)^exp(lngamma)-cycle^exp(lngamma)))
# tdtps<-data.table(death.risk,revision.risk.sp0,revision.risk.np1)
# #tdtps
# 
# 
# 
# #  Now create a transition matrix for the standard prosthesis arm
# #  We start with a three dimensional array in order to capture the time dependencies
# #  The 'pull' command gives the value of that element without the indexing (without pull the indexing gets messed up in the array)
# 
# SP0.tm<-array(data=0,dim=c(5,5,60))
# #  provideDimnames(SP.tm,sep="-",base=list(state.names,state.names,"time"))
# 
# 
# for (i in 1:60) {
#   SP0.tm[1,2,i]<-1-omrPTHR
#   SP0.tm[1,5,i]<-omrPTHR
#   SP0.tm[2,2,i]<-1-revision.risk.sp0[i]-death.risk.vector[i]
#   SP0.tm[2,3,i]<-revision.risk.sp0[i]
#   SP0.tm[2,5,i]<-death.risk.vector[i]
#   SP0.tm[3,4,i]<-1-omrRTHR-death.risk.vector[i]
#   SP0.tm[3,5,i]<-omrRTHR+death.risk.vector[i]
#   SP0.tm[4,3,i]<-rrr
#   SP0.tm[4,4,i]<-1-rrr-death.risk.vector[i]
#   SP0.tm[4,5,i]<-death.risk.vector[i]
#   SP0.tm[5,5,i]<-1
# 
# }
# 
# 
# 
# #  Now create a transition matrix for the new prosthesis arm
# #  We start with a three dimensional array in order to capture the time dependencies
# #  The 'pull' command gives the value of that element without the indexing (without pull the indexing gets messed up in the array)
# 
# NP1.tm<-array(data=0,dim=c(5,5,60))
# #  provideDimnames(SP.tm,sep="-",base=list(state.names,state.names,"time"))
# 
# 
# for (i in 1:60) {
#   NP1.tm[1,2,i]<-1-omrPTHR
#   NP1.tm[1,5,i]<-omrPTHR
#   NP1.tm[2,2,i]<-1-revision.risk.np1[i]-death.risk.vector[i]
#   NP1.tm[2,3,i]<-revision.risk.np1[i]
#   NP1.tm[2,5,i]<-death.risk.vector[i]
#   NP1.tm[3,4,i]<-1-omrRTHR-death.risk.vector[i]
#   NP1.tm[3,5,i]<-omrRTHR+death.risk.vector[i]
#   NP1.tm[4,3,i]<-rrr
#   NP1.tm[4,4,i]<-1-rrr-death.risk.vector[i]
#   NP1.tm[4,5,i]<-death.risk.vector[i]
#   NP1.tm[5,5,i]<-1
# }
# 
# 
# #  Create a trace for the standard prosthesis arm
# 
# trace.SP0<-matrix(data=NA,nrow=cycles,ncol=n.states)
# colnames(trace.SP0)<-state.names
# trace.SP0[1,]<-seed%*%SP0.tm[,,1]
# 
# for (i in 2:cycles) {
#   trace.SP0[i,]<-trace.SP0[i-1,]%*%SP0.tm[,,i]
# }
# #trace.SP0
# 
# #  Create a trace for the new prosthesis arm
# 
# trace.NP1<-matrix(data=NA,nrow=cycles,ncol=n.states)
# colnames(trace.NP1)<-state.names
# trace.NP1[1,]<-seed%*%NP1.tm[,,1]
# 
# for (i in 2:cycles) {
#   trace.NP1[i,]<-trace.NP1[i-1,]%*%NP1.tm[,,i]
# }
# #trace.NP1
# 
# #  Calculate QALYs & discounted QALYs in each treatment arm
# 
# QALYs.SP0<-trace.SP0%*%state.utilities
# #QALYs.SP0
# QALYs.NP1<-trace.NP1%*%state.utilities
# #QALYs.NP1
# undisc.QALYs.SP0<-colSums(QALYs.SP0)
# #undisc.QALYs.SP0
# undisc.QALYs.NP1<-colSums(QALYs.NP1)
# #undisc.QALYs.NP1
# 
# 
# disc.QALYs.SP0<-O.discount.factor%*%QALYs.SP0
# disc.QALYs.NP1<-O.discount.factor%*%QALYs.NP1
# #disc.QALYs.SP0
# #disc.QALYs.NP1
# 
# #  Calculate costs and discounted costs in each treatment arm
# 
# cost.SP0<-trace.SP0%*%state.costs
# #cost.SP0
# 
# undisc.cost.SP0<-colSums(cost.SP0)+cSP0
# #undisc.cost.SP0
# 
# cost.NP1<-trace.NP1%*%state.costs
# #cost.NP1
# undisc.cost.NP1<-colSums(cost.NP1)+cNP1
# #undisc.cost.NP1
# 
# 
# disc.cost.SP0<-C.discount.factor%*%cost.SP0+cSP0
# disc.cost.NP1<-C.discount.factor%*%cost.NP1+cNP1
# #disc.cost.SP0
# #disc.cost.NP1
# 
# #  Cost-effectiveness results
# 
# inc.cost<-disc.cost.NP1-disc.cost.SP0
# inc.QALYs<-disc.QALYs.NP1-disc.QALYs.SP0
# #icer<-inc.cost/inc.QALYs
# #inc.cost
# #inc.QALYs
# #icer
# 
# increments<-c(inc.QALYs,inc.cost)
# return(increments)
# 
# }
# 
# 
# # Use a loop to run simulations
# 
# # Set the number of simulations to run
# sim.runs <- 1000
# 
# simulation.results <- data.frame(matrix(0, sim.runs, 2))
# colnames(simulation.results)<-c("inc.QALYs","inc.costs")
# pb = txtProgressBar(min = 0, max = sim.runs, initial = 0, style = 3)
# 
# for(i in 1:sim.runs) {
#   setTxtProgressBar(pb,i)
#   simulation.results[i,] <- model.THR(60,0)
# }
# 
# plot(simulation.results$inc.QALYs,simulation.results$inc.cost)
# 
# # Mean results
# colMeans(simulation.results)

