#  Decision Modelling for Health Economic Evaluation
#  Advanced Course Exercise 1: SOLUTION FILE
#  Authors: Andrew Briggs, Jack Williams & Nichola Naylor

### Loading useful packages
library(data.table)
library(tidyr)
library(dplyr)

#########**** PARAMETERS *****######
#  Start by defining parameters
#  Demographics & Discount rates

age <- 60 ## set age group for analyses
male <- 0 ## set sex identified, 0 = female and 1 = male
        ## the specific number (0,1) becomes important for reasons you'll see further down the script
dr.c <- 0.06 ## set the discount rate for costs (6%)
dr.o <- 0.015 ## set the discount rate for outcomes (1.5%)

cycles <- 60 ## number of cycles

state.names <- c("P-THR","successP-THR","R-THR","successR-THR","Death")
n.states <- length(state.names)

#  Seed the starting states of the model
seed <- c(1,0,0,0,0) ## all people start in the first state

#  Transition probabilities
tp.PTHR2dead <- 0.02 ## Operative mortality rate (OMR) following primary THR
tp.RTHR2dead <- 0.02 ##Operative mortality rate (OMR) following revision THR
tp.rrr <- 0.04 ## Re-revision risk (assumed to be constant)

#  Costs
c.SP0 <- 394 ## Cost of standard prosthesis
c.NP1 <- 579 ## Cost of new prosthesis 1

c.primary <- 0  ## Cost of a primary THR procedure 
## Note that the cost of the primary procedure is excluded (set to 0): since both arms have this procedure it is assumed to net out of the incremental analysis.  However, if the model was to be used to estimate lifetime costs of THR it would be important to include.
c.success <- 0 ## Cost of one cycle in a 'success' state (primary or revision)
## Note for c.sucess There are assumed to be no ongoing monitoring costs for successful THR.  However, this parameter is included in case users want to change this assumption.

c.revision <- 5294 ## Cost of one cycle in the Revision THR state (national reference costs for revision hip or knee)

state.costs<-c(c.primary, c.success, c.revision,c.success,0) ## a vector with the costs for each state

# Life years
state.lys <- c(1,1,1,1,0)  ## a vector of life year effects for each state

#  Utilities
u.success.p <- 0.85 ## Utility score for having had a successful Primary THR
u.revision <- 0.30 ## Utility score during the revision period
u.success.r <- 0.75 ## Utility score for having a successful Revision THR

state.utilities <- c(0,u.success.p,u.revision,u.success.r,0) ## a vector with the utilities for each state

#### HAZARD FUNCTION & ASSOCIATED PARAMETERS #####

hazards <- read.csv("hazardfunction.csv") ## importing the hazard inputs from the regression analysis

## Coefficients - on the log hazard scale
r.lnlambda <- hazards$coefficient[1] ## Ancilliary parameter in Weibull distribution - equivalent to lngamma coefficient
r.cons <- hazards$coefficient[2] ##Constant in survival analysis for baseline hazard
r.ageC <- hazards$coefficient[3] ## Age coefficient in survival analysis for baseline hazard
r.maleC <- hazards$coefficient[4] ## Male coefficient in survival analysis for baseline hazard
r.NP1 <- hazards$coefficient[5]

gamma <- exp(r.lnlambda)
lambda <- exp(r.cons+age*r.ageC+male*r.maleC)
RR.NP1 <- exp(r.NP1)

##### LIFE TABLES #####
#  Read in the life table
life.table <- read.csv("life-table.csv") ## importing the life table csv inputs
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

####**** STANDARD *****#####

## defining the revision risks based on the parameters calculated above and cycle vector
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

#  Now create a transition matrix for the standard prosthesis arm
#  We start with a three dimensional array in order to capture the time dependencies
tm.SP0 <- array(data=0,dim=c(n.states, n.states, cycles),
                dimnames= list(state.names, state.names, 1:cycles)) ## an empty array of dimenions (number of states, number of states, number of cycles)
                   ## naming all dimensions

### create a loop that creates a time dependent transition matrix for each cycle
for (i in 1:cycles) {
  
  ## First we get the correct mortality risk for each cycle 
  mortality <- as.numeric(tdtps[i,..col.key]) 
  
  ## tranisitions out of P-THR
  
  tm.SP0["P-THR","Death",i] <- tp.PTHR2dead ## Primary THR either enter the death state or.. or..
  tm.SP0["P-THR","successP-THR",i] <- 1 - tp.PTHR2dead ## they go into the success THR state 
  
  ## transitions out of success-P-THR
  tm.SP0["successP-THR","R-THR",i] <- revision.risk.sp0[i] ## you could also refer to the corersponding tdtps column
  tm.SP0["successP-THR","Death",i] <- mortality
  tm.SP0["successP-THR","successP-THR",i] <- 1-revision.risk.sp0[i] - mortality
  
  ## transitions out of R-THR 
  tm.SP0["R-THR","Death",i] <- tp.RTHR2dead + mortality
  tm.SP0["R-THR","successR-THR",i] <- 1 - tp.RTHR2dead - mortality 
  
  ## transitions out of success-THR
  tm.SP0["successR-THR","R-THR",i] <- tp.rrr
  tm.SP0["successR-THR","Death",i] <- mortality
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

###Life Years####

lys.SP0 <- trace.SP0%*%state.lys
lys.SP0

undisc.lys.SP0 <- colSums(lys.SP0)
undisc.lys.SP0

###QALYS######

QALYs.SP0 <- trace.SP0%*%state.utilities
QALYs.SP0

undisc.QALYs.SP0 <- colSums(QALYs.SP0)
undisc.QALYs.SP0

## DISCOUNTING:
## this time we use 
discount.factor.o <- 1/(1+dr.o)^cycle.v ## many different methods to do this, this one simply multiplies the cycle vector with the discount formulae

discount.factor.o

disc.QALYs.SP0 <- discount.factor.o%*%QALYs.SP0
disc.QALYs.SP0

###COSTS###

cost.SP0 <- trace.SP0%*%state.costs
cost.SP0

undisc.cost.SP0 <- colSums(cost.SP0) + c.SP0
undisc.cost.SP0

## DISCOUNTING
discount.factor.c <- 1/(1+dr.c)^cycle.v

disc.cost.SP0 <- (discount.factor.c%*%cost.SP0) + c.SP0
disc.cost.SP0

##########**** NP1 *****######

tm.NP1 <- array(data=0,dim=c(n.states, n.states, cycles),
                dimnames= list(state.names, state.names, 1:cycles)) ## an empty array of dimenions (number of states, number of states, number of cycles)

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
  tm.NP1["successR-THR","Death",i] <- mortality
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

rowSums(trace.NP1)


###Life Years####
lys.NP1 <- trace.NP1%*%state.lys
lys.NP1

undisc.lys.NP1 <- colSums(lys.NP1)
undisc.lys.NP1

###QALYS######
QALYs.NP1 <- trace.NP1%*%state.utilities
QALYs.NP1

undisc.QALYs.NP1 <- colSums(QALYs.NP1)
undisc.QALYs.NP1

## DISCOUNTING:
## can use the same discount.factor.o as created previously 
disc.QALYs.NP1 <- discount.factor.o%*%QALYs.NP1
disc.QALYs.NP1

###COSTS###
cost.NP1 <- trace.NP1%*%state.costs
cost.NP1

undisc.cost.NP1 <- colSums(cost.NP1) + c.NP1
undisc.cost.NP1

## DISCOUNTING
disc.cost.NP1 <- (discount.factor.c%*%cost.NP1) + c.NP1
disc.cost.NP1


####****ANALYSIS****####

output <- c(inc.cost = disc.cost.NP1 - disc.cost.SP0,
            inc.qalys = disc.QALYs.NP1 - disc.QALYs.SP0,
            icer = NA)
output["icer"] <- output["inc.cost"]/output["inc.lys"]

output


