#  DM4HEE 
#  Exercise 3.5 - Replication of the THR model
#  Author: Andrew Briggs
#  Date created: 20 February 2021
#  Date last edit: 08 March 2021

### Loading useful packages
library(data.table)
library(tidyr)
library(dplyr)

#########**** PARAMETERS *****######
#  Start by defining parameters

#  Demographics

age<-60
male<-0

#  Transition probabilities

omrPTHR<-0.02
omrRTHR<-0.02
rrr<-0.04

#  Revision rates

cons<--5.490935
ageC<--0.0367022
maleC<-0.768536
NP1<--1.344474
lngamma<-0.3740968
sp0.LP<-cons+age*ageC+male*maleC
np1.LP<-cons+age*ageC+male*maleC+NP1

# ## NN suggestion to format like excel but maybe too much 
# # can link to "data.table" cheat sheets etc. of any
# # similar packages we use - I think data.table should be ok
# # in terms of above base R (?) but also happy to stick to base
# hazards <- as.data.table(read.csv("inputs/hazardfunction.csv"))
# cons <- hazards[explanatory.variables=="cons",coefficient]
# ageC <- hazards[explanatory.variables=="age",coefficient]
# maleC <- hazards[explanatory.variables=="male",coefficient]
# NP1 <- hazards[explanatory.variables=="NP1",coefficient]
# lngamma <- hazards[explanatory.variables=="lngamma",coefficient]

#  Costs
cPrimary<-0
cRevision<-5294
cSuccess<-0
cSP0<-394
cNP1<-579
state.costs<-c(cPrimary, cSuccess, cRevision,cSuccess,0)

#  Utilities

uSuccessP<-0.85
uSuccessR<-0.75
uRevision<-0.30
state.utilities<-c(0,uSuccessP,uRevision,uSuccessR,0)

#  Read in the life table

#life.table <- read.csv("life-table.csv")
# # this is how i've saved on my computer - but need to discuss how to set the files / folders to ensure this are appropriate 
# life.table <- read.csv("../inputs/life-table.csv")
# life.table<-data.table(life.table)

### if you open the "Rproject" file rather than the 
# individual R codes you no longer need to set a repository
# so here the above would be...
life.table <- read.csv("inputs/life-table.csv")
life.table <- as.data.table(life.table)
## bug it's currently naming the first column strangely so for now setnames to avoid error later on
colnames(life.table) <- c("Index","Males","Female")

#  Other parameters
cDR<-0.06
oDR<-0.015

#  Seed the starting states of the model
seed<-c(1,0,0,0,0)

#  Set the total number of cycles to run and name the states
cycles<-60

state.names<-c("P-THR","successP-THR","R-THR","successR-THR","Death")
n.states<-length(state.names)

#  Create revision and death risk as function of age
cycle<-1:cycles
current.age<-age+cycle
current.age
death.risk<-data.table(current.age)
setkey(life.table,"Index")
setkey(death.risk,"current.age")
death.risk<-life.table[death.risk, roll=TRUE]
revision.risk.sp0<-1-exp(exp(sp0.LP)*((cycle-1)^exp(lngamma)-cycle^exp(lngamma)))
revision.risk.np1<-1-exp(exp(np1.LP)*((cycle-1)^exp(lngamma)-cycle^exp(lngamma)))

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

for (i in 1:(cycles-1)) {
  trace.SP0[i+1,]<-trace.SP0[i,]%*%SP0.tm[,,i+1]
}
trace.SP0

#  Create a trace for the new prosthesis arm

trace.NP1<-matrix(data=NA,nrow=cycles,ncol=n.states)
colnames(trace.NP1)<-state.names
trace.NP1[1,]<-seed%*%NP1.tm[,,1]

for (i in 1:(cycles-1)) {
  trace.NP1[i+1,]<-trace.NP1[i,]%*%NP1.tm[,,i+1] ## error here
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