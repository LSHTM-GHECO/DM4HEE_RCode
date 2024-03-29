#  Decision Modelling for Health Economic Evaluation
#  Advanced Course Exercise 2: TEMPLATE FILE
#  Authors: Andrew Briggs, Jack Williams & Nichola Naylor

### !!! set working directory as the folder this is stored in
## added this in to allow for the running of instruction pdf knitting
## whilst reading in data from the same subfolder
## students can ignore if not re-knitting the pdfs, just make sure data files
## are stored in the same file as template/solution files

# require("rstudioapi")  
# setwd(dirname(getActiveDocumentContext()$path)) # Set working directory to source file 


#  Reading the data needed from csv files
hazards <- read.csv("hazardfunction.csv", header=TRUE) ## importing the hazard inputs from the regression analysis

cov.55 <- read.csv("cov55.csv",row.names=1,header=TRUE) ## importing the covariance data

life.table <- read.csv("life-table.csv", header=TRUE) ## importing the life table 

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

tp.PTHR2dead <-   rbeta(n = , shape1 = , shape2 = )   ## Operative mortality rate  (OMR) following primary THR
# since we assume the same shape parameters for RTHR : 
tp.RTHR2dead <-   rbeta( )   ## Operative mortality rate (OMR) following revision THR

a.rrr <-     ## alpha value for re-revision risk
b.rrr <-     ## beta value for re-revision risk
tp.rrr <-    ## Re-revision risk transition probability (beta distribution)

tp.PTHR2dead
tp.RTHR2dead
tp.rrr

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

mn <- c(mn.lngamma, mn.cons,mn.ageC,mn.maleC,mn.NP1) ## vector of mean values from the regression analysis

cholm <- t(chol(t(cov.55))) ## lower triangle of the Cholesky decomposition

z <- rnorm() ## 5 random draws from the normal distribution

Tz <- %*%  ## Tz which is the Cholesky matrix multiplied by the 5 random draws (z)

x <-   ## mn plus Tz

r.lngamma<-x[1,1] 
r.cons<-x[2,1]
r.ageC<-x[3,1]
r.maleC<-x[4,1]
r.NP1<-x[5,1]

gamma <- exp(  )    ##Ancilliary parameter in Weibull distribution
lambda <- exp(  )    ##Lambda parameter survival analysis
RR.NP1 <- exp(  )    ##Relative risk of revision for new prosthesis 1 compared to standard

##### LIFE TABLES #####

colnames(life.table) <- c("Age","Index","Males","Female") ## making sure column names are correct

cycle.v <-       ## a vector of cycle numbers 1 - 60
current.age <-   ## a vector of cohort age throughout the model
current.age

## Creating a table that has every age of the cohort plus death risks associated with that age
# This finds the position of age, within the life table 
interval <- findInterval(current.age, life.table$Index)
# These positions can then be used to subset the appropriate values from life.table
death.risk <- data.frame(age = current.age, 
                         males = life.table[interval,3],
                         females = life.table[interval,4])


#####***** MARKOV MODEL ****#####

#### TRANS AND TRACE ######
#  Now create a transition matrix for the standard prosthesis arm and NP1 arm
#  We start with a three dimensional array in order to capture the time dependencies
revision.risk.sp0 <- 1- exp(lambda * ((cycle.v-1) ^gamma-cycle.v ^gamma))
revision.risk.np1 <- 1- exp(lambda * RR.NP1 * ((cycle.v-1) ^gamma-cycle.v ^gamma))

revision.risk.sp0 ## the time dependent risk of revision for standard treatment
revision.risk.np1 ## the time dependent risk of revision for NP1

# combining risks into a time-dependent transition probability data.frame
tdtps <- data.frame(death.risk, revision.risk.sp0, revision.risk.np1)
tdtps

## creating an indicator which selects the death risk column depending on the sex the model is being run on
col.key <- 3-male ## 3 indicates the 3rd column of tdps (which is female risk of death)
## when male=1 (i.e. male selected as sex) this becomes the 2nd column (which is male risk of death)

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
  tm.SP0["successP-THR","R-THR",i] <- revision.risk.sp0[i]
  tm.SP0["successP-THR","Death",i] <- death.risk[i,col.key]
  tm.SP0["successP-THR","successP-THR",i] <- 1-revision.risk.sp0[i] - death.risk[i,col.key]
  ## transitions out of R-THR 
  tm.SP0["R-THR","Death",i] <- tp.RTHR2dead + death.risk[i,col.key]
  tm.SP0["R-THR","successR-THR",i] <- 1 - tp.RTHR2dead - death.risk[i,col.key] 
  ## transitions out of success-THR
  tm.SP0["successR-THR","R-THR",i] <- tp.rrr
  tm.SP0["successR-THR","Death",i] <- death.risk[i,col.key]
  tm.SP0["successR-THR","successR-THR",i] <- 1 - tp.rrr - death.risk[i,col.key]
  
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
  tm.NP1["successP-THR","Death",i] <- death.risk[i, col.key] 
  tm.NP1["successP-THR","successP-THR",i] <- 1 - revision.risk.np1[i] - death.risk[i, col.key] 
  ## transitions out of R-THR 
  tm.NP1["R-THR","Death",i] <- tp.RTHR2dead + death.risk[i, col.key] 
  tm.NP1["R-THR","successR-THR",i] <- 1 - tp.RTHR2dead - death.risk[i, col.key]  
  ## transitions out of success-THR
  tm.NP1["successR-THR","R-THR",i] <- tp.rrr
  tm.NP1["successR-THR","Death",i] <- death.risk[i, col.key] 
  tm.NP1["successR-THR","successR-THR",i] <- 1 - tp.rrr - death.risk[i, col.key] 
  
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

discount.factor.c <- 1/(1+dr.c)^cycle.v  ## discount factor matrix for costs
discount.factor.c

disc.cost.SP0 <- (discount.factor.c%*%cost.SP0) + c.SP0
disc.cost.SP0

# NP1 ARM
cost.NP1 <- trace.NP1%*%state.costs
cost.NP1

undisc.cost.NP1 <- colSums(cost.NP1) + c.NP1
undisc.cost.NP1

disc.cost.NP1 <- (discount.factor.c%*%cost.NP1) + c.NP1 ## the same discount factor matrix can be used for costs for both arms
disc.cost.NP1

###QALYS######

# STANDARD ARM
QALYs.SP0 <- trace.SP0%*%state.utilities
QALYs.SP0

undisc.QALYs.SP0 <- colSums(QALYs.SP0)
undisc.QALYs.SP0

discount.factor.o <- 1/(1+dr.o)^cycle.v  ## discount factor matrix for outcomes
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

output <- c(inc.cost = , ## incremental cost
            inc.qalys =  ,  ## incremental effect 
            icer = NA)  ## incremental cost-effectiveness ratioe
output["icer"] <- 
  
output

round(output,3) ## we can round the output table for printing also
