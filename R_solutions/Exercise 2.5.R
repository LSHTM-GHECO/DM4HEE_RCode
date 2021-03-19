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
              ## but this is stating the first value in the vector is "A.AsympHIV"
state.names
### TRANSITION PROBABILITIES ############

alpha.A2A<-1251 ## Counts of transitions from A to A (see Table 2.5)
alpha.A2B<-350 ## Counts of transitions from A  to B
alpha.A2C<-116 ## Counts of transitions from A to C
alpha.A2D<-17 ## Counts of transitions from A to D
alpha.B2B<-731 ## Counts of transitions from B to B
alpha.B2C<-512
alpha.B2D<-15
alpha.C2C<-1312
alpha.C2D<-437

A.sum <- 1734 ## total counts of transitions out of A
B.sum <- 1258 ## total counts of transitions out of B
C.sum <- 1749 ## total counts of transitions out of C  

tp.A2A<-alpha.A2A/A.sum ## transition probability of A to A
tp.A2B<-alpha.A2B/A.sum ## transition probability of A to B
tp.A2C<-alpha.A2C/A.sum  
tp.A2D<-alpha.A2D/A.sum
tp.B2B<-alpha.B2B/B.sum ## transition probability of B to B
tp.B2C<-alpha.B2C/B.sum ## transition probability of B to C
tp.B2D<-alpha.B2D/B.sum
tp.C2C<-alpha.C2C/C.sum ## transition probability of C to C
tp.C2D<-alpha.C2D/C.sum ## transition probability of C to D

# ### Note you could input the numbers directly 
# ## e.g
# tp.A2A <- 1251/1734 
# ## but for understanding of where the numbers come frome
# ## we asked you to work through the calculations of these numbers
# ## also note if we wanted to calculate complements we can do this using:
# beta.A2A <- A.sum-alpha.A2A

### COSTS ####################
c.dmca<-1701  ## Direct medical costs associated with state A
c.dmcb<-1774 ##Direct medical costs associated with state B
c.dmcc<-6948  ## Direct medical costs associated with stateC
C.dmc<-c(c.dmca, c.dmcb, c.dmcc,0) ## A vector storing the direct costs associated with each state
                          ## the order is important as these will be multiplied according to 
                          ## matrix multiplication 
                          ## (e.g. first value in dmc - direct medical cost of A - will be multiplied by 
                          ## first value in a matrix that represents number of cases in state A) 
c.ccca<-1055 ## Community care costs associated with state A
c.cccb<-1278 ## Community care costs associated with state B
c.cccc<-2059 ## Community care costs associated with state C
c.ccc<-c(ccca,cccb,cccc,0) ## A vector storing the community costs associated with each state

#  Drug costs
c.AZT<-2278  ### Zidovudine drug cost
c.LAM <-2086.5 ## Lamivudine drug cost 
c.azt<-c(c.AZT,c.AZT,c.AZT,0) ## A vector of Lamivudine drug costs per state
                              ## Notice how the final death state has £0 cost
c.lam<-c(c.LAM,c.LAM,c.LAM,0) ## A vector of Lamivudine drug costs per state

### OTHER PARAMETERS #######
RR<-0.509 ## Treatment effect (RR)
dr.c<-0.06 ## Annual discount rate - costs (%)
dr.o<-0  ## Annual discount rate - benefits (%) 

#  Seed the starting states of the model
seed<-c(1,0,0,0) ## i.e. everyone starts in State A

####**** MARKOV MODEL ****######
#  Set the total number of cycles for the model to run
cycles<-20 ## i.e. we want to run the model for each year for 20 years

#  Now create a transition matrix for the AZT arm
#  This shows the probability of transitioning from one state to another 
n.states<-length(state.names)
A.AsympHIV.AZT<-c(tp.A2A,tp.A2B,tp.A2C,tp.A2D) ## all of the transitions out of A in one vector, with each value corresponding to a transition to a different state (A, B, C, D)
B.SympHIV.AZT<-c(0,tp.B2B,tp.B2C,tp.B2D) 
C.AIDS.AZT<-c(0,0,tp.C2C,tp.C2D)
D.Death<-c(0,0,0,1) ## as nobody transitions out of dead the transition probability of staying in dead, once in dead, is equal to 1
tm.AZT<-matrix(data=rbind(A.AsympHIV.AZT,B.SympHIV.AZT,C.AIDS.AZT,D.Death),nrow=n.states,ncol=n.states)
rownames(tm.AZT)<-state.names ## renaming the matrix row names
colnames(tm.AZT)<-state.names ## renaming the matrix column names
tm.AZT 

#  Create a trace for the AZT arm
#  This captures the number of people in each state at any one time
trace.AZT<-matrix(data=NA,nrow=cycles+1,ncol=n.states) ## the length of the matrix is equivalent to the number of cycles
                                                      ## we add 1 row to the cycle length for the seed row (cycle 0)
colnames(trace.AZT)<-state.names
trace.AZT[1,]<-seed ## set the first row as the seed population 

## Let's see what the first few rows of the Markov trace looks like:
head(trace.AZT) ## the head() function returns the first 6 rows of a matrix (or data.frame)
# tail(trace.AZT) ## the tail() function returns the last 6 rows.

## building the trace function requires you to loop through each row
## and multiply the number of people in A at time t-1 with transition probabilities related to A
## to get the number of people in different states at time t
trace.AZT[2,]<-trace.AZT[1,]%*%tm.AZT
head(trace.AZT) 

for (i in 3:(cycles+1)) {
  trace.AZT[i,]<-trace.AZT[i-1,]%*%tm.AZT
}
trace.AZT

rowSums(trace.AZT)
## note to check - why rowSums(trace.AZT)==1 is only true for 
# 1st and last values ? rounding error?

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

for (i in 3:cycles) {
  trace.comb[i,]<-trace.comb[i-1,]%*%tm.AZT
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

### output table
output <- c(inc.cost=disc.cost.comb-disc.cost.AZT,
            inc.LYs=disc.LYs.comb-disc.LYs.AZT,
            icer =inc.cost/inc.LYs)
output
