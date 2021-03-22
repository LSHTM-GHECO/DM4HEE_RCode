#  DM4HEE 
#  Exercise 3.5 - Replication of THR model
#  Author: Andrew Briggs
#  Edited by: Jack Williams & Nichola Naylor
#  Date created: 20 February 2021
#  Date last edit: 22 March 2021

### Loading useful packages
library(data.table)
library(tidyr)
library(dplyr)

#########**** PARAMETERS *****######
#  Start by defining parameters

#  Demographics & Discount rates

age <-60 ## set age group for analyses
male <-0 ## set sex identified, 0 = male and 1 = female
        ## the specific number (0,1) becomes important for reasons you'll see further down the script
dr.c <-0.06 ## set the discount rate for costs (6%)
dr.o <-0.015 ## set the discount rate for outcomes (15%)

cycles<-60

state.names<-c("P-THR","successP-THR","R-THR","successR-THR","Death")
n.states<-length(state.names)

#  Create revision and death risk as function of age
cycle<-1:cycles

#  Transition probabilities
tp.PTHR2dead<-0.02 ## Operative mortality rate following primary THR
tp.RTHR2dead <-0.02 ##Operative mortality rate following revision THR
tp.rrr <-0.04 ## Re-revision risk (assumed to be constant)

#  Costs
c.primary<-0  ## Cost of a primary THR procedure - 
## Note that the cost of the primary procedure is excluded (set to 0): since both arms have this procedure it is assumed to net out of the incremental analysis.  However, if the model was to be used to estimate lifetime costs of THR it would be important to include.
c.revision<-5294 ## Cost of one cycle in the Revision THR state (national reference costs for revision hip or knee)
c.success<-0 ## Cost of one cycle in a 'success' state (primary or revision)
## Note for u.sucess There are assumed to be no ongoing monitoring costs for successful THR.  However, this parameter is included in case users want to change this assumption.
c.SP0<-394 ## Cost of standard prosthesis
c.NP1<-579 ## Cost of new prosthesis 1
state.costs<-c(c.Primary, c.Success, c.Revision,c.Success,0) ## a vector with the costs for each state

#  Utilities
u.SuccessP<-0.85 ## Utility score for having had a successful Primary THR
u.SuccessR<-0.75 ## Utility score for having a successful Revision THR
u.Revision<-0.30 ## Utility score during the revision period
state.utilities<-c(0,u.SuccessP,u.Revision,u.SuccessR,0) ## a vector with the utilities for each state

#### HAZARD FUNCTION & ASSOCIATED PARAMETERS #####
hazards <- read.csv("inputs/hazardfunction.csv")

## Coefficients - on the log hazard scale
r.cons<- hazards$coefficient[2] ##Constant in survival analysis for baseline hazard
r.ageC<- hazards$coefficient[3] ## Age coefficient in survival analysis for baseline hazard
r.maleC<- hazards$coefficient[4] ## Male coefficient in survival analysis for baseline hazard

## Coefficients - calculations needed
r.gamma <- hazards$coefficient[1] ## Ancilliary parameter in Weibull distribution - equivalent to lngamma coefficient
r.lambda <- exp(cons+age*r.ageC+male*r.maleC)
NP1<- exp(hazards$coefficient[5]) 

####!!! got to here
sp0.LP<-cons+age*ageC+male*maleC
np1.LP<-cons+age*ageC+male*maleC+NP1
revision.risk.sp0<-1-exp(exp(sp0.LP)*((cycle-1)^exp(lngamma)-cycle^exp(lngamma)))
revision.risk.np1<-1-exp(exp(np1.LP)*((cycle-1)^exp(lngamma)-cycle^exp(lngamma)))


##### LIFE TABLES #####
#  Read in the life table
life.table <- read.csv("inputs/life-table.csv")
## bug it's currently naming the first column strangely so for now setnames to avoid error later on
colnames(life.table) <- c("Index","Males","Female")

####**** STANDARD TREATMENT *****#####
#  Seed the starting states of the model
seed<-c(1,0,0,0,0)

current.age<-age+cycle
current.age
death.risk<-data.table(current.age)
setkey(life.table,"Index")
setkey(death.risk,"current.age")
death.risk<-life.table[death.risk, roll=TRUE]

# these are all merged into a data table but then causes problems below adding to matrix - why not just use the main sources
tdtps<-data.table(death.risk,revision.risk.sp0,revision.risk.np1)
tdtps
mr<-3-male 

#  Now create a transition matrix for the standard prosthesis arm
#  We start with a three dimensional array in order to capture the time dependencies
#  The 'pull' command gives the value of that element without the indexing (without pull the indexing gets messed up in the array)
SP0.tm<-array(data=0,dim=c(5,5,60))

## updates 
n.t <- 60 ## easily adjustable then once defined if agree, need to replace throughout
SP0.tm <- array(data=0,dim=c(5,5,n.t))
SP0.tm <- provideDimnames(SP0.tm,sep="-",base=list(state.names,state.names,"time"))

for (i in 1:60) {
  
  mortality <- as.numeric(tdtps[i,..mr]) 
  
  ## as.numeric seemingly faster (v. slightly) than pull - check I've done the right calc in "verstorisation_speed_example.R"
  
  SP0.tm[1,2,i]<-1-omrPTHR ## NB: to come back to once finalised format - needs more annotations on what these are
  SP0.tm[1,5,i]<-omrPTHR
  SP0.tm[2,2,i]<-1-revision.risk.sp0[i]-mortality
  SP0.tm[2,3,i]<-revision.risk.sp0[i]
  SP0.tm[2,5,i]<-mortality
  SP0.tm[3,4,i]<-1-omrRTHR-mortality
  SP0.tm[3,5,i]<-omrRTHR+mortality
  SP0.tm[4,3,i]<-rrr
  SP0.tm[4,4,i]<-1-rrr-mortality
  SP0.tm[4,5,i]<-mortality
  SP0.tm[5,5,i]<-1
}

SP0.tm

#  Now create a transition matrix for the new prosthesis arm
#  We start with a three dimensional array in order to capture the time dependencies
#  The 'pull' command gives the value of that element without the indexing (without pull the indexing gets messed up in the array)

##########**** NP1 *****######
NP1.tm<-array(data=0,dim=c(5,5,60))
# NP1.tm <- provideDimnames(NP1.tm,sep="-",base=list(state.names,state.names,"time"))

# to be edited based on decision on matrix
for (i in 1:60) {
  
  mortality <- as.numeric(tdtps[i,..mr]) 
  
  NP1.tm[1,2,i]<-1-omrPTHR
  NP1.tm[1,5,i]<-omrPTHR
  NP1.tm[2,2,i]<-1-revision.risk.np1[i]-mortality
  NP1.tm[2,3,i]<-revision.risk.np1[i]
  NP1.tm[2,5,i]<-mortality
  NP1.tm[3,4,i]<-1-omrRTHR-mortality
  NP1.tm[3,5,i]<-omrRTHR+mortality
  NP1.tm[4,3,i]<-rrr
  NP1.tm[4,4,i]<-1-rrr-mortality
  NP1.tm[4,5,i]<-mortality
  NP1.tm[5,5,i]<-1
  
}

#  Create a trace for the standard prosthesis arm

trace.SP0<-matrix(data=NA,nrow=cycles,ncol=n.states)
colnames(trace.SP0)<-state.names
trace.SP0[1,]<-seed%*%SP0.tm[,,1]

for (i in 2:cycles) {
  trace.SP0[i,]<-trace.SP0[i-1,]%*%SP0.tm[,,i]
}
trace.SP0

#  Create a trace for the new prosthesis arm

trace.NP1<-matrix(data=NA,nrow=cycles,ncol=n.states)
colnames(trace.NP1)<-state.names
trace.NP1[1,]<-seed%*%NP1.tm[,,1]

for (i in 2:cycles) {
  trace.NP1[i,]<-trace.NP1[i-1,]%*%NP1.tm[,,i] 
}
trace.NP1

#  Calculate QALYs & discounted QALYs in each treatment arm

QALYs.SP0<-trace.SP0%*%state.utilities
QALYs.SP0
QALYs.NP1<-trace.NP1%*%state.utilities
QALYs.NP1
undisc.QALYs.SP0<-colSums(QALYs.SP0)
undisc.QALYs.SP0
undisc.QALYs.NP1<-colSums(QALYs.NP1)
undisc.QALYs.NP1

O.discount.factor<-matrix(data=NA,nrow=1,ncol=cycles)
for (i in 1:cycles) {
  O.discount.factor[1,i]<-1/(1+oDR)^i
}

# a much simpler alternative here (also doesnt need to be a matrix)
O.discount.factor <- 1/(1+oDR)^cycle
O.discount.factor

disc.QALYs.SP0<-O.discount.factor%*%QALYs.SP0
disc.QALYs.NP1<-O.discount.factor%*%QALYs.NP1
disc.QALYs.SP0
disc.QALYs.NP1

#  Calculate costs and discounted costs in each treatment arm

cost.SP0<-trace.SP0%*%state.costs
cost.SP0

undisc.cost.SP0<-colSums(cost.SP0)+cSP0
undisc.cost.SP0

cost.NP1<-trace.NP1%*%state.costs
cost.NP1
undisc.cost.NP1<-colSums(cost.NP1)+cNP1
undisc.cost.NP1

C.discount.factor<-matrix(data=NA,nrow=1,ncol=cycles)
for (i in 1:cycles) {
  C.discount.factor[1,i]<-1/(1+cDR)^i
}

# alternative 
C.discount.factor <- 1/(1+cDR)^cycle
C.discount.factor

disc.cost.SP0<-C.discount.factor%*%cost.SP0+cSP0
disc.cost.NP1<-C.discount.factor%*%cost.NP1+cNP1
disc.cost.SP0
disc.cost.NP1

#  Cost-effectiveness results

inc.cost<-disc.cost.NP1-disc.cost.SP0
inc.QALYs<-disc.QALYs.NP1-disc.QALYs.SP0
icer<-inc.cost/inc.QALYs
inc.cost
inc.QALYs
icer

# ### NN suggestion - store as a data.frame output?
# output <- data.frame(inc.cost=disc.cost.comb-disc.cost.AZT,
#                     inc.LYs=disc.LYs.comb-disc.LYs.AZT,
#                     icer =inc.cost/inc.LYs)
# output

## !! NB to come back to...at some point need to add in error messages
# e.g. if not summing to 1 over transitions etc.
# (look at previous R learning docs)