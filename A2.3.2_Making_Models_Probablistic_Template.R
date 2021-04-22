#  Decision Modelling for Health Economic Evaluation
#  Advanced Course Exercise 2: TEMPLATE FILE
#  Authors: Andrew Briggs, Jack Williams & Nichola Naylor

### Loading useful packages
library(data.table)
library(tidyr)
library(dplyr)

#  Reading the data needed from csv files
hazards <- read.csv("hazardfunction.csv", header=TRUE) ## importing the hazard inputs from the regression analysis

cov.55 <- read.csv("cov55.csv",row.names=1,header=TRUE) ## importing the covariance data

life.table <- read.csv("life-table.csv", header=TRUE) ## importing the life table 
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

#### PROBABLISTIC PARAMETERS #####

###  Transition probabilities

a.PTHR2dead <-     ## alpha value for operative mortality from primary surgery
b.PTHR2dead <-     ## beta value for operative mortality from primary surgery

tp.PTHR2dead <-      ## Operative mortality rate  (OMR) following primary THR
# since we assume the same shape parameters for RTHR : 
tp.RTHR2dead <-      ## Operative mortality rate (OMR) following revision THR

a.rrr <-     ## alpha value for re-revision risk
b.rrr <-     ## beta value for re-revision risk
tp.rrr <-    ## Re-revision risk transition probability

tp.PTHR2dead
tp.RTHR2dead
tp.rrr

# calculating deterministic mean and SE as a comparison for these 3 transition probabilities 
mn.PTHR2dead <-     ## mean value for tp.PTHR2dead
se.PTHR2dead <-      ## standard error for tp.PTHR2dead

# the values are the same for revision surgery, as the alpha and beta are assumed to be the same
mn.PTHR2dead
se.PTHR2dead 

mn.rrr <-         ## mean value for Re-revision risk 
se.rrr <-         ## standard error value for Re-revision risk 

##  Costs
c.primary <- 0  ## Cost of a primary THR procedure - 
## Note that the cost of the primary procedure is excluded (set to 0): since both arms have this procedure it is assumed to net out of the incremental analysis.  However, if the model was to be used to estimate lifetime costs of THR it would be important to include.

mn.cRevision <-    ## mean cost of revision surgery
se.cRevision <-    ## standard error of cost of revision surgery

a.cRevision <-      ## alpha value for cost of revision surgery 
b.cRevision <-      ## beta value for cost of revision surgery
c.revision <- rgamma(n =  ,shape =  , scale =  )    ## Gamma distribution draw for cost of revision surgery

c.success <- 0 ## Cost of one cycle in a 'success' state (primary or revision)
## Note for c.sucess There are assumed to be no ongoing monitoring costs for successful THR.  However, this parameter is included in case users want to change this assumption.

## remember that c.SP0 and c.NP1 are fixed and were defined at the beginning of this exercise

state.costs<-   ## a vector with the costs for each state


##  Utilities

# primary prosthesis
mn.uSuccessP <-      ## mean utility value for successful primary prosthesis
se.uSuccessP <-      ## standard errror utility value for successful primary prosthesis

ab.uSuccessP <-  ## estimating alpha plus beta (ab)
a.uSuccessP <-    ## estimating alpha (a)
b.uSuccessP <-     ## estimating beta (b)
uSuccessP <-       ## drawing from the Beta distribution based on a and b

## revision surgery
mn.uSuccessR <-    ## mean utility value for having a successful Revision THR
se.uSuccessR <-    ## standard error utility value for having a successful Revision THR

ab.uSuccessR <-       ## alpha + beta (ab)
a.uSuccessR <-        ## alpha (a)
b.uSuccessR <-        ## beta(b)
uSuccessR <-          ## drawing from the Beta distribution based on a and b

## during the revision period 
mn.uRevision <-       ## mean utility score during the revision period
se.uRevision <-       ## standard error utility score during the revision period

ab.uRevision <-       ## alpha + beta (ab)
a.uRevision  <-       ## alpha (a)
b.uRevision  <-       ## beta(b)
uRevision    <-       ## drawing from the Beta distribution based on a and b

state.utilities <-    ## a vector of health state utilities

##  Hazard function ####

## Coefficients - on the log hazard scale
mn.lngamma <- hazards$coefficient[1] ## lngamma coefficient
mn.cons <- hazards$coefficient[2] ##Constant 
mn.ageC <- hazards$coefficient[3] ## Age coefficient
mn.maleC <- hazards$coefficient[4] ## Male coefficient 
mn.NP1 <- hazards$coefficient[5] ## NP1 coefficient

mn<-c(mn.lngamma, mn.cons,mn.ageC,mn.maleC,mn.NP1) ## vector of mean values from the regression analysis

cholm <- t(chol(t(cov.55))) ## lower triangle of the Cholesky decomposition

z <-   ## 5 random draws from the normal distribution

Tz <-    ## Tz which is the Cholesky matrix multiplied by the 5 random draws

x <-   ## mn plus Tz

r.lngamma<-x[1,1] 
r.cons<-x[2,1]
r.ageC<-x[3,1]
r.maleC<-x[4,1]
r.NP1<-x[5,1]

gamma <-      ##Ancilliary parameter in Weibull distribution
lambda <-     ##Lambda parameter survival analysis
RR.NP1 <-     ##Relative risk of revision for new prosthesis 1 compared to standard

##### LIFE TABLES #####

colnames(life.table) <- c("Age","Index","Males","Female") ## making sure column names are correct

cycle.v <-       ## a vector of cycle numbers 1 - 60
current.age <-   ## a vector of cohort age throughout the model
current.age

## creating a table that has every age of the cohort plus death risks associated with that age
life.table <-       ## turning life.table into a data.table 
death.risk <-       ## turning current age into a data.table 
setkey(life.table,"Index") ## using the setkey function (read about it by typing in ?setkey in the console)
setkey(death.risk,"current.age") ## using the setkey function for death.risk to sort and set current.age as the key
death.risk <-       ## joining life.table and death.risk by the key columns, rolling forward between index values
death.risk

#####***** MARKOV MODEL ****#####

#### TRANS AND TRACE ######
#  Now create a transition matrix for the standard prosthesis arm and NP1 arm
#  We start with a three dimensional array in order to capture the time dependencies
revision.risk.sp0 <- 
revision.risk.np1 <- 

# combining risks into a time-dependent transition probability data.table
tdtps <- 
  
## creating an indicator which selects the death risk column depending on the sex the model is being run on
col.key <-      ## 4 indicates the 4th column of tdps (which is female risk of death)
## when male=1 (i.e. male selected as sex) this becomes the 3rd column (which is male risk of death)

#   STANDARD ARM
#  Now create a transition matrix for the standard prosthesis arm
#  We start with a three dimensional array in order to capture the time dependencies
tm.SP0 <-     ## an empty array of dimenions (number of states, number of states, number of cycles)

### create a loop that creates a time dependent transition matrix for each cycle
for (i in 1:cycles) {
  
  mortality <- as.numeric(tdtps[i,..col.key]) 
  ## tranisitions out of P-THR
  tm.SP0["P-THR","Death",i] <- 
  tm.SP0["P-THR","successP-THR",i] <-
  ## transitions out of success-P-THR
  tm.SP0["successP-THR","R-THR",i] <-
  tm.SP0["successP-THR","Death",i] <- 
  tm.SP0["successP-THR","successP-THR",i] <- 
  ## transitions out of R-THR 
  tm.SP0["R-THR","Death",i] <- 
  tm.SP0["R-THR","successR-THR",i] <- 
  ## transitions out of success-THR
  tm.SP0["successR-THR","R-THR",i] <- 
  tm.SP0["successR-THR","Death",i] <- 
  tm.SP0["successR-THR","successR-THR",i] <-
  
  tm.SP0["Death","Death",i] <- 1 ## no transitions out of death
}

tm.SP0

#  Create a trace for the standard prosthesis arm
trace.SP0 <- 
colnames(trace.SP0) <- state.names

trace.SP0[1,] <- 

for (i in 2:cycles) {
  
}
trace.SP0

rowSums(trace.SP0)

#  NP1 ARM
tm.NP1 <-    ## an empty array of dimenions (number of states, number of states, number of cycles)

### create a loop that creates a time dependent transition matrix for each cycle
for (i in 1:cycles) {
  
  
}

tm.NP1

#  Create a trace for the standard prosthesis arm
trace.NP1 <- 
colnames(trace.NP1) <- state.names

trace.NP1[1,] <- 

for (i in 2:cycles) {
  
  mortality <- 
  ## tranisitions out of P-THR
  tm.NP1["P-THR","Death",i] <-    ## Primary THR either enter the death state or.. or..
  tm.NP1["P-THR","successP-THR",i] <-    ## they go into the success THR state 
  ## transitions out of success-P-THR
  tm.NP1["successP-THR","R-THR",i] <-    ## revision risk with NP1 treatment arm 
  tm.NP1["successP-THR","Death",i] <-  
  tm.NP1["successP-THR","successP-THR",i] <-  
  ## transitions out of R-THR 
  tm.NP1["R-THR","Death",i] <-   
  tm.NP1["R-THR","successR-THR",i] <- 1 -   
  ## transitions out of success-THR
  tm.NP1["successR-THR","R-THR",i] <-   
  tm.NP1["successR-THR",5,i] <- 
  tm.NP1["successR-THR","successR-THR",i] <-
  
  tm.NP1["Death","Death",i] <- 1 ## no transitions out of death
  
}
trace.NP1

rowSums(trace.SP0)

#### COST #####

# STANDARD ARM
cost.SP0 <-  ## undiscounted costs by cycle for standard treatment
cost.SP0

undisc.cost.SP0 <-   ## total undiscounted costs (plus one-off cost from cycle 0)
undisc.cost.SP0

discount.factor.c <-    ## discount factor matrix for costs
discount.factor.c

disc.cost.SP0 <-    ## total discounted costs (plus one-off cost from cycle 0)
disc.cost.SP0

# NP1 ARM 
cost.NP1 <-      ## undiscounted costs by cycle for NP1
cost.NP1

undisc.cost.NP1 <-     ## total undiscounted costs for NP1 (plus one-off cycle0 costs)
undisc.cost.NP1

disc.cost.NP1 <-      ## total discounted costs for NP1 (plus one-off cycle0 costs)
disc.cost.NP1

###QALYS######

# STANDARD ARM
QALYs.SP0 <-         ## undiscounted QALYs per cycle
QALYs.SP0

undisc.QALYs.SP0 <-     ## total undiscounted QALYs 
undisc.QALYs.SP0

discount.factor.o <-       ## discount factor matrix for outcomes
discount.factor.o

disc.QALYs.SP0 <-     ## total discounted QALYs
disc.QALYs.SP0

# NP1 ARM
QALYs.NP1 <-   ## undiscounted QALYs per cycle
QALYs.NP1

undisc.QALYs.NP1 <-     ## total undiscounted QALYs 
undisc.QALYs.NP1

disc.QALYs.NP1 <-       ## total discounted QALYs
disc.QALYs.NP1

####****ANALYSIS****####

output <- c(inc.cost = , ## incremental cost
            inc.lys =  ,  ## incremental effect 
            icer = NA)  ## incremental cost-effectiveness ratioe
output["icer"] <- 
  
output

round(output,2) ## we can round the output table for printing also
