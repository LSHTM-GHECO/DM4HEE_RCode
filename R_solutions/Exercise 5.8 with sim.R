#  DM4HEE 
#  Exercise 5.8 - Introducing a third prosthesis into the THR model
#  Author: Jack Williams / Nichola Naylor
#  Date created: 15 March 2021
#  Date last edit: 15 March 2021

library(data.table)

#  Read in the life table and covariance table

life.table <- read.csv("inputs/life-table.csv")
life.table<-data.table(life.table)
cov.55<-read.csv("inputs/cov55_NP2.csv",row.names=1,header=TRUE) # New covariance matrix with additional comparator


# Define fixed parameters outside of the model to avoid repition

cDR<-0.06
oDR<-0.015

#  Seed the starting states of the model
seed<-c(1,0,0,0,0)

#  Set the total number of cycles to run and name the states
cycles<-60

state.names<-c("P-THR","successP-THR","R-THR","successR-THR","Death")
n.states<-length(state.names)


# O.discount.factor<-matrix(data=NA,nrow=1,ncol=cycles)
# for (i in 1:cycles) {
#   O.discount.factor[1,i]<-1/(1+oDR)^i
# }
# O.discount.factor
O.discount.factor <- matrix(1/(1+oDR) ^ c(1:cycles), nrow = 1, ncol = cycles)
O.discount.factor


# C.discount.factor<-matrix(data=NA,nrow=1,ncol=cycles)
# for (i in 1:cycles) {
#   C.discount.factor[1,i]<-1/(1+cDR)^i
# }
# C.discount.factor
C.discount.factor <- matrix(1/(1+cDR) ^ c(1:cycles), nrow = 1, ncol = cycles)
C.discount.factor


model.THR<-function(age, male) {
  
  #  Defining parameters
  
  #  Demographics
  
  #age<-60
  #male<-0
  
  #  Transition probabilities
  
  omrPTHR<-rbeta(1,2,98)
  omrRTHR<-rbeta(1,2,98)
  rrr<-rbeta(1,4,96)
  
  #  Revision rates
  
  mn.cons<--5.490935
  mn.ageC<--0.0367022
  mn.maleC<-0.768536
  mn.NP1<--1.344474
  mn.NP2 <- -1.6687 # added comparator  
  mn.lngamma<-0.3740968
  mn<-c(mn.lngamma, mn.cons,mn.ageC,mn.maleC,mn.NP1,mn.NP2)
  Tchol <-chol(cov.55)
  z<-rnorm(6,0,1)
  x<-mn + Tchol %*%z
  
  lngamma<-x[1,1]
  cons<-x[2,1]
  ageC<-x[3,1]
  maleC<-x[4,1]
  NP1<-x[5,1]
  NP2<-x[6,1]
  
  sp0.LP<-cons+age*ageC+male*maleC
  np1.LP<-cons+age*ageC+male*maleC+NP1
  np2.LP<-cons+age*ageC+male*maleC+NP2 # Add comparator
  
  #  Costs
  
  cPrimary<-0
  mn.cRevision<-5294
  se.cRevision<-1487
  a.cRevision<-(mn.cRevision/se.cRevision)^2
  b.cRevision<-(se.cRevision^2)/mn.cRevision
  cRevision<-rgamma(1,shape=a.cRevision,scale=b.cRevision)
  cSuccess<-0
  cSP0<-394
  cNP1<-579
  cNP2 <- 788 ## Add cost
  state.costs<-c(cPrimary, cSuccess, cRevision,cSuccess,0)
  
  #  Utilities
  
  mn.uSuccessP<-0.85
  se.uSuccessP<-0.03
  ab.uSuccessP<-mn.uSuccessP*(1-mn.uSuccessP)/(se.uSuccessP^2)
  a.uSuccessP<-mn.uSuccessP*ab.uSuccessP
  b.uSuccessP<-a.uSuccessP*(1-mn.uSuccessP)/mn.uSuccessP
  uSuccessP<-rbeta(1,a.uSuccessP,b.uSuccessP)
  
  mn.uSuccessR<-0.75
  se.uSuccessR<-0.04
  ab.uSuccessR<-mn.uSuccessR*(1-mn.uSuccessR)/(se.uSuccessR^2)
  a.uSuccessR<-mn.uSuccessR*ab.uSuccessR
  b.uSuccessR<-a.uSuccessR*(1-mn.uSuccessR)/mn.uSuccessR
  uSuccessR<-rbeta(1,a.uSuccessR,b.uSuccessR)
  
  mn.uRevision<-0.30
  se.uRevision<-0.03
  ab.uRevision<-mn.uRevision*(1-mn.uRevision)/(se.uRevision^2)
  a.uRevision<-mn.uRevision*ab.uRevision
  b.uRevision<-a.uRevision*(1-mn.uRevision)/mn.uRevision
  uRevision<-rbeta(1,a.uRevision,b.uRevision)
  
  state.utilities<-c(0,uSuccessP,uRevision,uSuccessR,0)
  
  
  
  #  Create revision and death risk as function of age
  
  mr<-3-male
  cycle<-1:cycles
  current.age<-age+cycle
  #current.age
  death.risk<-data.table(current.age)
  setkey(life.table,"Index")
  setkey(death.risk,"current.age")
  death.risk<-life.table[death.risk, roll=TRUE]
  death.risk.vector <- as.vector(as.matrix(death.risk[,..mr]))
  
  revision.risk.sp0<-1-exp(exp(sp0.LP)*((cycle-1)^exp(lngamma)-cycle^exp(lngamma)))
  revision.risk.np1<-1-exp(exp(np1.LP)*((cycle-1)^exp(lngamma)-cycle^exp(lngamma)))
  revision.risk.np2<-1-exp(exp(np2.LP)*((cycle-1)^exp(lngamma)-cycle^exp(lngamma)))
  #tdtps<-data.table(death.risk,revision.risk.sp0,revision.risk.np1)
  #tdtps
  

  #  Now create a transition matrix for the standard prosthesis arm
  #  We start with a three dimensional array in order to capture the time dependencies

  
  SP0.tm<-array(data=0,dim=c(5,5,cycles))
  #  provideDimnames(SP.tm,sep="-",base=list(state.names,state.names,"time"))
  
  
  for (i in 1:cycles) {
    SP0.tm[1,2,i]<-1-omrPTHR
    SP0.tm[1,5,i]<-omrPTHR
    SP0.tm[2,2,i]<-1-revision.risk.sp0[i]-death.risk.vector[i]
    SP0.tm[2,3,i]<-revision.risk.sp0[i]
    SP0.tm[2,5,i]<-death.risk.vector[i]
    SP0.tm[3,4,i]<-1-omrRTHR-death.risk.vector[i]
    SP0.tm[3,5,i]<-omrRTHR+death.risk.vector[i]
    SP0.tm[4,3,i]<-rrr
    SP0.tm[4,4,i]<-1-rrr-death.risk.vector[i]
    SP0.tm[4,5,i]<-death.risk.vector[i]
    SP0.tm[5,5,i]<-1
    
  }
  
  
  
  #  Now create a transition matrix for the new prosthesis arm
  #  We start with a three dimensional array in order to capture the time dependencies
  #  The 'pull' command gives the value of that element without the indexing (without pull the indexing gets messed up in the array)
  
  NP1.tm<-array(data=0,dim=c(5,5,cycles))
  #  provideDimnames(SP.tm,sep="-",base=list(state.names,state.names,"time"))
  
  
  for (i in 1:cycles) {
    NP1.tm[1,2,i]<-1-omrPTHR
    NP1.tm[1,5,i]<-omrPTHR
    NP1.tm[2,2,i]<-1-revision.risk.np1[i]-death.risk.vector[i]
    NP1.tm[2,3,i]<-revision.risk.np1[i]
    NP1.tm[2,5,i]<-death.risk.vector[i]
    NP1.tm[3,4,i]<-1-omrRTHR-death.risk.vector[i]
    NP1.tm[3,5,i]<-omrRTHR+death.risk.vector[i]
    NP1.tm[4,3,i]<-rrr
    NP1.tm[4,4,i]<-1-rrr-death.risk.vector[i]
    NP1.tm[4,5,i]<-death.risk.vector[i]
    NP1.tm[5,5,i]<-1
  }
  
  

  NP2.tm<-array(data=0,dim=c(5,5,cycles))
  #  provideDimnames(SP.tm,sep="-",base=list(state.names,state.names,"time"))
  
  
  for (i in 1:cycles) {
    NP2.tm[1,2,i]<-1-omrPTHR
    NP2.tm[1,5,i]<-omrPTHR
    NP2.tm[2,2,i]<-1-revision.risk.np2[i]-death.risk.vector[i]
    NP2.tm[2,3,i]<-revision.risk.np2[i]
    NP2.tm[2,5,i]<-death.risk.vector[i]
    NP2.tm[3,4,i]<-1-omrRTHR-death.risk.vector[i]
    NP2.tm[3,5,i]<-omrRTHR+death.risk.vector[i]
    NP2.tm[4,3,i]<-rrr
    NP2.tm[4,4,i]<-1-rrr-death.risk.vector[i]
    NP2.tm[4,5,i]<-death.risk.vector[i]
    NP2.tm[5,5,i]<-1
  }
  
  
  
  #  Create a trace for the standard prosthesis arm
  
  trace.SP0<-matrix(data=NA,nrow=cycles,ncol=n.states)
  colnames(trace.SP0)<-state.names
  trace.SP0[1,]<-seed%*%SP0.tm[,,1]
  
  for (i in 2:cycles) {
    trace.SP0[i,]<-trace.SP0[i-1,]%*%SP0.tm[,,i]
  }
  #trace.SP0
  
  #  Create a trace for the first prosthesis arm
  
  trace.NP1<-matrix(data=NA,nrow=cycles,ncol=n.states)
  colnames(trace.NP1)<-state.names
  trace.NP1[1,]<-seed%*%NP1.tm[,,1]
  
  for (i in 2:cycles) {
    trace.NP1[i,]<-trace.NP1[i-1,]%*%NP1.tm[,,i]
  }
  #trace.NP1
  
  
  #  Create a trace for the second prosthesis arm
  
  trace.NP2<-matrix(data=NA,nrow=cycles,ncol=n.states)
  colnames(trace.NP2)<-state.names
  trace.NP2[1,]<-seed%*%NP2.tm[,,1]
  
  for (i in 1:(cycles-1)) {
    trace.NP2[i+1,]<-trace.NP2[i,]%*%NP2.tm[,,i]
  }
  #trace.NP2
  
  
  #  Calculate QALYs & discounted QALYs in each treatment arm
  
  QALYs.SP0<-trace.SP0%*%state.utilities
  #QALYs.SP0
  QALYs.NP1<-trace.NP1%*%state.utilities
  #QALYs.NP1
  QALYs.NP2<-trace.NP2%*%state.utilities
  #QALYs.NP2
  undisc.QALYs.SP0<-colSums(QALYs.SP0)
  #undisc.QALYs.SP0
  undisc.QALYs.NP1<-colSums(QALYs.NP1)
  #undisc.QALYs.NP1
  undisc.QALYs.NP2<-colSums(QALYs.NP2)
  #undisc.QALYs.NP2
  
  disc.QALYs.SP0<-O.discount.factor%*%QALYs.SP0
  disc.QALYs.NP1<-O.discount.factor%*%QALYs.NP1
  disc.QALYs.NP2<-O.discount.factor%*%QALYs.NP2
  #disc.QALYs.SP0
  #disc.QALYs.NP1
  #disc.QALYs.NP2
  
  #  Calculate costs and discounted costs in each treatment arm
  
  cost.SP0<-trace.SP0%*%state.costs
  #cost.SP0
  undisc.cost.SP0<-colSums(cost.SP0)+cSP0
  #undisc.cost.SP0
  
  cost.NP1<-trace.NP1%*%state.costs
  #cost.NP1
  undisc.cost.NP1<-colSums(cost.NP1)+cNP1
  #undisc.cost.NP1
  
  cost.NP2<-trace.NP2%*%state.costs
  #cost.NP2
  undisc.cost.NP2<-colSums(cost.NP2)+cNP2
  #undisc.cost.NP2
  
  disc.cost.SP0<-C.discount.factor%*%cost.SP0+cSP0
  disc.cost.NP1<-C.discount.factor%*%cost.NP1+cNP1
  disc.cost.NP2<-C.discount.factor%*%cost.NP2+cNP2
  #disc.cost.SP0
  #disc.cost.NP1
  #disc.cost.NP2
  
  #  Cost-effectiveness results - need to fix 
  
  results <- c(disc.QALYs.SP0, disc.cost.SP0,
               disc.QALYs.NP1, disc.cost.NP1,
               disc.QALYs.NP2, disc.cost.NP2)
  
  #inc.cost<-disc.cost.NP1-disc.cost.SP0
  #inc.QALYs<-disc.QALYs.NP1-disc.QALYs.SP0
  #icer<-inc.cost/inc.QALYs
  #inc.cost
  #inc.QALYs
  #icer
  
  #increments<-c(inc.QALYs,inc.cost)
  return(results)
  
}

sim.runs <- 100

simulation.results <- data.frame(matrix(0, sim.runs, 6))
colnames(simulation.results)<-c("QALY SP0","Cost SP0", "QALY NP1","Cost NP1", "QALY NP2","Cost NP2")
pb = txtProgressBar(min = 0, max = sim.runs, initial = 0, style = 3)

for(i in 1:sim.runs) {
  setTxtProgressBar(pb,i)  
  simulation.results[i,] <- model.THR(60,0)
}



# Mean results across simulations
apply(simulation.results, 2, mean)
