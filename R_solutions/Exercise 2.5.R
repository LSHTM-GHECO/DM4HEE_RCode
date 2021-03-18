#  DM4HEE 
#  Exercise 2.5 - Replication of the HIV/AIDS model
#  Author: Andrew Briggs
#  Edited by: Nichola Naylor & Jack Williams 
#  Date created: 19 February 2021
#  Date last edit: 18 March 2021

####****PARAMETERS****#####
#  Start by defining parameters
state.names<-c("A.AsympHIV","B.SympHIV","C.AIDS","D.Death") 
              ## the ordering is important here, you will see why as we go 

### TRANSITION PROBABILITIES ############
A.sum <- 1734 ## total counts of transitions out of A
B.sum <- 1258 ## total counts of transitions out of B
C.sum <- 1749 ## total counts of transitions out of C
  
tpA2A<-1251/A.sum ## transitions A to A
tpA2B<-350/A.sum ## transitions A to B and so on
tpA2C<-116/A.sum  
tpA2D<-17/A.sum
tpB2B<-731/B.sum
tpB2C<-512/B.sum
tpB2D<-15/B.sum
tpC2C<-1312/C.sum 
tpC2D<-437/C.sum 

### COSTS ####################
dmca<-1701  ## Direct medical costs associated with state A
dmcb<-1774 ##Direct medical costs associated with state B
dmcc<-6948  ## Direct medical costs associated with stateC
dmc<-c(dmca, dmcb, dmcc,0) ## A vector storing the direct costs associated with each state
                          ## the order is important as these will be multiplied according to 
                          ## matrix multiplication 
                          ## (e.g. first value in dmc - direct medical cost of A - will be multiplied by 
                          ## first value in a matrix that represents number of cases in state A) 
ccca<-1055 ## Community care costs associated with state A
cccb<-1278 ## Community care costs associated with state B
cccc<-2059 ## Community care costs associated with state C
ccc<-c(ccca,cccb,cccc,0) ## A vector storing the community costs associated with each state

#  Drug costs
cAZT<-2278  ### Zidovudine drug cost
cLam<-2086.5 ## Lamivudine drug cost 
azt<-c(cAZT,cAZT,cAZT,0) ## A vector of Lamivudine drug costs per state
Lam<-c(cLam,cLam,cLam,0) ## A vector of Lamivudine drug costs per state

### OTHER PARAMETERS #######
RR<-0.509 ## Treatment effect (RR)
cDR<-0.06 ## Annual discount rate - costs (%)
oDR<-0  ## Annual discount rate - benefits (%) 

#  Seed the starting states of the model
seed<-c(1,0,0,0) ## i.e. everyone starts in State A

#  Set the total number of cycles to run
cycles<-20

####**** MARKOV MODEL ****######
#  Now create a transition matrix for the AZT arm

n.states<-length(state.names)
A.AsympHIV.AZT<-c(tpA2A,tpA2B,tpA2C,tpA2D) ## all of the transitions out of A
B.SympHIV.AZT<-c(0,tpB2B,tpB2C,tpB2D)
C.AIDS.AZT<-c(0,0,tpC2C,tpC2D)
D.Death<-c(0,0,0,1)
tm.AZT<-matrix(data=rbind(A.AsympHIV.AZT,B.SympHIV.AZT,C.AIDS.AZT,D.Death),nrow=n.states,ncol=n.states)
rownames(tm.AZT)<-state.names
colnames(tm.AZT)<-state.names
tm.AZT

#  Create a trace for the AZT arm

trace.AZT<-matrix(data=NA,nrow=cycles,ncol=n.states)
colnames(trace.AZT)<-state.names
trace.AZT[1,]<-seed%*%tm.AZT

## NN could add som more code which helps them check/see whats happening
# head(trace.AZT)

for (i in 1:(cycles-1)) {
  trace.AZT[i+1,]<-trace.AZT[i,]%*%tm.AZT
}
trace.AZT

#  Create a transition matrix for the combination therapy arm
A.AsympHIV.comb<-c(1-(tpA2B+tpA2C+tpA2D)*RR,tpA2B*RR,tpA2C*RR,tpA2D*RR)
B.SympHIV.comb<-c(0,1-(tpB2C+tpB2D)*RR,tpB2C*RR,tpB2D*RR)
C.AIDS.comb<-c(0,0,1-tpC2D*RR,tpC2D*RR)

tm.comb<-matrix(data=rbind(A.AsympHIV.comb,B.SympHIV.comb,C.AIDS.comb,D.Death),nrow=n.states,ncol=n.states)
rownames(tm.comb)<-state.names
colnames(tm.comb)<-state.names
tm.comb

#  Create a trace for the combination arm

trace.comb<-matrix(data=NA,nrow=cycles,ncol=n.states)
colnames(trace.comb)<-state.names
trace.comb[1,]<-seed%*%tm.comb
trace.comb[2,]<-trace.comb[1,]%*%tm.comb

for (i in 2:(cycles-1)) {
  trace.comb[i+1,]<-trace.comb[i,]%*%tm.AZT
}
trace.comb

#  Calculate life years & discounted life years in each treatment arm

LYs<-c(1,1,1,0)

LYs.AZT<-trace.AZT%*%LYs
LYs.AZT
LYs.comb<-trace.comb%*%LYs
LYs.comb
undisc.LYs.AZT<-colSums(LYs.AZT)
undisc.LYs.AZT
undisc.LYs.comb<-colSums(LYs.comb)
undisc.LYs.comb

O.discount.factor<-matrix(data=NA,nrow=1,ncol=cycles)
for (i in 1:cycles) {
  O.discount.factor[1,i]<-1/(1+oDR)^i
}
O.discount.factor

## JW alternative 
# O.discount.factor <- matrix(1/(1+oDR) ^ c(1:cycles), nrow = 1, ncol = cycles)

disc.LYs.AZT<-O.discount.factor%*%LYs.AZT
disc.LYs.comb<-O.discount.factor%*%LYs.comb
disc.LYs.AZT
disc.LYs.comb

#  Calculate costs and discounted costs in each treatment arm

cost.AZT<-trace.AZT%*%dmc+trace.AZT%*%ccc+trace.AZT%*%azt
cost.AZT

undisc.cost.AZT<-colSums(cost.AZT)
undisc.cost.AZT

cost.comb<-trace.comb%*%dmc+trace.comb%*%ccc+trace.comb%*%azt
cost.comb[1,1]<-cost.comb[1,1]+(trace.comb[1,1]+trace.comb[1,2]+trace.comb[1,3])*cLam
cost.comb[2,1]<-cost.comb[2,1]+(trace.comb[2,1]+trace.comb[2,2]+trace.comb[2,3])*cLam
cost.comb

C.discount.factor<-matrix(data=NA,nrow=1,ncol=cycles)
for (i in 1:cycles) {
  C.discount.factor[1,i]<-1/(1+cDR)^i
}
C.discount.factor

## JW alternative 
# C.discount.factor <- matrix(1/(1+cDR) ^ c(1:cycles), nrow = 1, ncol = cycles)

disc.cost.AZT<-C.discount.factor%*%cost.AZT
disc.cost.comb<-C.discount.factor%*%cost.comb
disc.cost.AZT
disc.cost.comb

#######**** ANALYSIS *****#####
#  Cost-effectiveness results
inc.cost<-disc.cost.comb-disc.cost.AZT
inc.LYs<-disc.LYs.comb-disc.LYs.AZT
icer<-inc.cost/inc.LYs
inc.cost
inc.LYs
icer

### JW suggestion - for this one may be easier as vector/matrix? i.e.
output <- c(inc.cost=disc.cost.comb-disc.cost.AZT,
            inc.LYs=disc.LYs.comb-disc.LYs.AZT,
            icer =inc.cost/inc.LYs)
output
