#  DM4HEE 
#  Exercise 4.7 - Making the HIV/AIDS model probabilistic
#  Author: Andrew Briggs
#  Date created: 22 February 2021
#  Date last edit: 12 March 2021

library(plyr)
library(ggplot2)


# Define fixed values prior to running the model, to avoid repition

#  Drug costs
cAZT<-2278
cLam<-2086.5
azt<-c(cAZT,cAZT,cAZT,0)
Lam<-c(cLam,cLam,cLam,0)

#  Seed the starting states of the model
seed<-c(1,0,0,0)

#  Set the total number of cycles to run
cycles<-20


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


model.HIV<-function() {

# Start by defining probabilistic parameters

#  Transition probabilities

gd.A2A<-rgamma(1,shape=1251,scale=1)
gd.A2B<-rgamma(1,shape=350,scale=1)
gd.A2C<-rgamma(1,shape=116,scale=1)
gd.A2D<-rgamma(1,shape=17,scale=1)

tpA2A<-gd.A2A/(gd.A2A+gd.A2B+gd.A2C+gd.A2D)
tpA2B<-gd.A2B/(gd.A2A+gd.A2B+gd.A2C+gd.A2D)
tpA2C<-gd.A2C/(gd.A2A+gd.A2B+gd.A2C+gd.A2D)
tpA2D<-gd.A2D/(gd.A2A+gd.A2B+gd.A2C+gd.A2D)

gd.B2B<-rgamma(1,shape=731,scale=1)
gd.B2C<-rgamma(1,shape=512,scale=1)
gd.B2D<-rgamma(1,shape=15,scale=1)
tpB2B<-gd.B2B/(gd.B2B+gd.B2C+gd.B2D)
tpB2C<-gd.B2C/(gd.B2B+gd.B2C+gd.B2D)
tpB2D<-gd.B2D/(gd.B2B+gd.B2C+gd.B2D)

gd.C2C<-rgamma(1,shape=1312,scale=1)
gd.C2D<-rgamma(1,shape=437,scale=1)
tpC2C<-gd.C2C/(gd.C2C+gd.C2D)
tpC2D<-gd.C2D/(gd.C2C+gd.C2D)


#  Costs

mn.dmca<-1701
se.dmca<-1701
a.dmca<-(mn.dmca/se.dmca)^2
b.dmca<-(se.dmca^2)/mn.dmca
dmca<-rgamma(1,shape=a.dmca,scale=b.dmca)

mn.dmcb<-1774
se.dmcb<-1774
a.dmcb<-(mn.dmcb/se.dmcb)^2
b.dmcb<-(se.dmcb^2)/mn.dmcb
dmcb<-rgamma(1,shape=a.dmcb,scale=b.dmcb)

mn.dmcc<-6948
se.dmcc<-6948
a.dmcc<-(mn.dmcc/se.dmcc)^2
b.dmcc<-(se.dmcc^2)/mn.dmcc
dmcc<-rgamma(1,shape=a.dmcc,scale=b.dmcc)

dmc<-c(dmca, dmcb, dmcc,0)

mn.ccca<-1055
se.ccca<-1055
a.ccca<-(mn.ccca/se.ccca)^2
b.ccca<-(se.ccca^2)/mn.ccca
ccca<-rgamma(1,shape=a.ccca,scale=b.ccca)

mn.cccb<-1278
se.cccb<-1278
a.cccb<-(mn.cccb/se.cccb)^2
b.cccb<-(se.cccb^2)/mn.cccb
cccb<-rgamma(1,shape=a.cccb,scale=b.cccb)

mn.cccc<-2059
se.cccc<-2059
a.cccc<-(mn.cccc/se.cccc)^2
b.cccc<-(se.cccc^2)/mn.cccc
cccc<-rgamma(1,shape=a.cccc,scale=b.cccc)


ccc<-c(ccca,cccb,cccc,0)

#dmc
#ccc



#  Other parameters

mn.RR<-0.509
lnRR<-log(mn.RR)
se.lnRR<-(log(0.710)-log(0.365))/(2*1.96)
RR<-exp(rnorm(1,lnRR,se.lnRR))
RR
cDR<-0.06
oDR<-0



#  Now create a transition matrix for the AZT arm

state.names<-c("A.AsympHIV","B.SympHIV","C.AIDS","D.Death")
n.states<-length(state.names)
A.AsympHIV.AZT<-c(tpA2A,tpA2B,tpA2C,tpA2D)
B.SympHIV.AZT<-c(0,tpB2B,tpB2C,tpB2D)
C.AIDS.AZT<-c(0,0,tpC2C,tpC2D)
D.Death<-c(0,0,0,1)
tm.AZT<-matrix(data=rbind(A.AsympHIV.AZT,B.SympHIV.AZT,C.AIDS.AZT,D.Death),nrow=n.states,ncol=n.states)
rownames(tm.AZT)<-state.names
colnames(tm.AZT)<-state.names
#tm.AZT

#  Create a trace for the AZT arm

trace.AZT<-matrix(data=NA,nrow=cycles,ncol=n.states)
colnames(trace.AZT)<-state.names
trace.AZT[1,]<-seed%*%tm.AZT

for (i in 1:(cycles-1)) {
  trace.AZT[i+1,]<-trace.AZT[i,]%*%tm.AZT
}
#trace.AZT

#  Create a transition matrix for the combination therapy arm

A.AsympHIV.comb<-c(1-(tpA2B+tpA2C+tpA2D)*RR,tpA2B*RR,tpA2C*RR,tpA2D*RR)
B.SympHIV.comb<-c(0,1-(tpB2C+tpB2D)*RR,tpB2C*RR,tpB2D*RR)
C.AIDS.comb<-c(0,0,1-tpC2D*RR,tpC2D*RR)

tm.comb<-matrix(data=rbind(A.AsympHIV.comb,B.SympHIV.comb,C.AIDS.comb,D.Death),nrow=n.states,ncol=n.states)
rownames(tm.comb)<-state.names
colnames(tm.comb)<-state.names
#tm.comb

#  Create a trace for the combination arm

trace.comb<-matrix(data=NA,nrow=cycles,ncol=n.states)
colnames(trace.comb)<-state.names
trace.comb[1,]<-seed%*%tm.comb
trace.comb[2,]<-trace.comb[1,]%*%tm.comb

for (i in 2:(cycles-1)) {
  trace.comb[i+1,]<-trace.comb[i,]%*%tm.AZT
}
#trace.comb

#  Calculate life years & discounted life years in each treatment arm

LYs<-c(1,1,1,0)

LYs.AZT<-trace.AZT%*%LYs
#LYs.AZT
LYs.comb<-trace.comb%*%LYs
#LYs.comb
undisc.LYs.AZT<-colSums(LYs.AZT)
#undisc.LYs.AZT
undisc.LYs.comb<-colSums(LYs.comb)
#undisc.LYs.comb


disc.LYs.AZT<-O.discount.factor%*%LYs.AZT
disc.LYs.comb<-O.discount.factor%*%LYs.comb
#disc.LYs.AZT
#disc.LYs.comb

#  Calculate costs and discounted costs in each treatment arm

cost.AZT<-trace.AZT%*%dmc+trace.AZT%*%ccc+trace.AZT%*%azt
#cost.AZT

undisc.cost.AZT<-colSums(cost.AZT)
#undisc.cost.AZT

cost.comb<-trace.comb%*%dmc+trace.comb%*%ccc+trace.comb%*%azt
cost.comb[1,1]<-cost.comb[1,1]+(trace.comb[1,1]+trace.comb[1,2]+trace.comb[1,3])*cLam
cost.comb[2,1]<-cost.comb[2,1]+(trace.comb[2,1]+trace.comb[2,2]+trace.comb[2,3])*cLam
#cost.comb


disc.cost.AZT<-C.discount.factor%*%cost.AZT
disc.cost.comb<-C.discount.factor%*%cost.comb
#disc.cost.AZT
#disc.cost.comb

#  Cost-effectiveness results

inc.cost<-disc.cost.comb-disc.cost.AZT
inc.LYs<-disc.LYs.comb-disc.LYs.AZT
# icer<-inc.cost/inc.LYs
# inc.cost
# inc.LYs
# icer

increments<-c(inc.LYs,inc.cost)
return(increments)

}

replicate(1000,model.HIV())
simulation.results<-rdply(1000,model.HIV(),.id=NULL)
colnames(simulation.results)<-c("inc.LYs","inc.costs")
plot(simulation.results$inc.LYs,simulation.results$inc.cost)



# Alternative approaches (quicker and avoids plyr package)

t0 <- Sys.time()

replicate(1000,model.HIV())
simulation.results<-rdply(1000,model.HIV(),.id=NULL)

t1 <- Sys.time()

simulation.results <- matrix(0, 1000, 2)
for(i in 1:1000) simulation.results[i,] <- model.HIV() 

t2 <- Sys.time()

t1 - t0
t2 - t1

#### NN agree loop easier

## for this first exercise do we want to consider also showing 
# an output of all the runs for variables - like seen on 
# 4.7 Markov Model excel solutions.. suggestion below:

#####***** test for full data table return *****########

# model.HIV.allvalues<-function() {
  # Start by defining probabilistic parameters
  #  Transition probabilities
  gd.A2A<-rgamma(1,shape=1251,scale=1)
  gd.A2B<-rgamma(1,shape=350,scale=1)
  gd.A2C<-rgamma(1,shape=116,scale=1)
  gd.A2D<-rgamma(1,shape=17,scale=1)
  
  tpA2A<-gd.A2A/(gd.A2A+gd.A2B+gd.A2C+gd.A2D)
  tpA2B<-gd.A2B/(gd.A2A+gd.A2B+gd.A2C+gd.A2D)
  tpA2C<-gd.A2C/(gd.A2A+gd.A2B+gd.A2C+gd.A2D)
  tpA2D<-gd.A2D/(gd.A2A+gd.A2B+gd.A2C+gd.A2D)
  
  gd.B2B<-rgamma(1,shape=731,scale=1)
  gd.B2C<-rgamma(1,shape=512,scale=1)
  gd.B2D<-rgamma(1,shape=15,scale=1)
  tpB2B<-gd.B2B/(gd.B2B+gd.B2C+gd.B2D)
  tpB2C<-gd.B2C/(gd.B2B+gd.B2C+gd.B2D)
  tpB2D<-gd.B2D/(gd.B2B+gd.B2C+gd.B2D)
  
  gd.C2C<-rgamma(1,shape=1312,scale=1)
  gd.C2D<-rgamma(1,shape=437,scale=1)
  tpC2C<-gd.C2C/(gd.C2C+gd.C2D)
  tpC2D<-gd.C2D/(gd.C2C+gd.C2D)
  #  Costs
  mn.dmca<-1701
  se.dmca<-1701
  a.dmca<-(mn.dmca/se.dmca)^2
  b.dmca<-(se.dmca^2)/mn.dmca
  dmca<-rgamma(1,shape=a.dmca,scale=b.dmca)
  mn.dmcb<-1774
  se.dmcb<-1774
  a.dmcb<-(mn.dmcb/se.dmcb)^2
  b.dmcb<-(se.dmcb^2)/mn.dmcb
  dmcb<-rgamma(1,shape=a.dmcb,scale=b.dmcb)
  mn.dmcc<-6948
  se.dmcc<-6948
  a.dmcc<-(mn.dmcc/se.dmcc)^2
  b.dmcc<-(se.dmcc^2)/mn.dmcc
  dmcc<-rgamma(1,shape=a.dmcc,scale=b.dmcc)
  dmc<-c(dmca, dmcb, dmcc,0)
  
  mn.ccca<-1055
  se.ccca<-1055
  a.ccca<-(mn.ccca/se.ccca)^2
  b.ccca<-(se.ccca^2)/mn.ccca
  ccca<-rgamma(1,shape=a.ccca,scale=b.ccca)
  mn.cccb<-1278
  se.cccb<-1278
  a.cccb<-(mn.cccb/se.cccb)^2
  b.cccb<-(se.cccb^2)/mn.cccb
  cccb<-rgamma(1,shape=a.cccb,scale=b.cccb)
  mn.cccc<-2059
  se.cccc<-2059
  a.cccc<-(mn.cccc/se.cccc)^2
  b.cccc<-(se.cccc^2)/mn.cccc
  cccc<-rgamma(1,shape=a.cccc,scale=b.cccc)
  ccc<-c(ccca,cccb,cccc,0)
  
  #dmc
  #ccc
  #  Other parameters
  
  mn.RR<-0.509
  lnRR<-log(mn.RR)
  se.lnRR<-(log(0.710)-log(0.365))/(2*1.96)
  RR<-exp(rnorm(1,lnRR,se.lnRR))
  RR
  cDR<-0.06
  oDR<-0
  
  #  Now create a transition matrix for the AZT arm
  state.names<-c("A.AsympHIV","B.SympHIV","C.AIDS","D.Death")
  n.states<-length(state.names)
  A.AsympHIV.AZT<-c(tpA2A,tpA2B,tpA2C,tpA2D)
  B.SympHIV.AZT<-c(0,tpB2B,tpB2C,tpB2D)
  C.AIDS.AZT<-c(0,0,tpC2C,tpC2D)
  D.Death<-c(0,0,0,1)
  tm.AZT<-matrix(data=rbind(A.AsympHIV.AZT,B.SympHIV.AZT,C.AIDS.AZT,D.Death),nrow=n.states,ncol=n.states)
  rownames(tm.AZT)<-state.names
  colnames(tm.AZT)<-state.names
  #tm.AZT
  
  #  Create a trace for the AZT arm
  trace.AZT<-matrix(data=NA,nrow=cycles,ncol=n.states)
  colnames(trace.AZT)<-state.names
  trace.AZT[1,]<-seed%*%tm.AZT
  
  for (i in 1:(cycles-1)) {
    trace.AZT[i+1,]<-trace.AZT[i,]%*%tm.AZT
  }
  #trace.AZT
  #  Create a transition matrix for the combination therapy arm
  A.AsympHIV.comb<-c(1-(tpA2B+tpA2C+tpA2D)*RR,tpA2B*RR,tpA2C*RR,tpA2D*RR)
  B.SympHIV.comb<-c(0,1-(tpB2C+tpB2D)*RR,tpB2C*RR,tpB2D*RR)
  C.AIDS.comb<-c(0,0,1-tpC2D*RR,tpC2D*RR)
  
  tm.comb<-matrix(data=rbind(A.AsympHIV.comb,B.SympHIV.comb,C.AIDS.comb,D.Death),nrow=n.states,ncol=n.states)
  rownames(tm.comb)<-state.names
  colnames(tm.comb)<-state.names
  #tm.comb
  
  #  Create a trace for the combination arm
  
  trace.comb<-matrix(data=NA,nrow=cycles,ncol=n.states)
  colnames(trace.comb)<-state.names
  trace.comb[1,]<-seed%*%tm.comb
  trace.comb[2,]<-trace.comb[1,]%*%tm.comb
  
  for (i in 2:(cycles-1)) {
    trace.comb[i+1,]<-trace.comb[i,]%*%tm.AZT
  }
  #trace.comb
  
  #  Calculate life years & discounted life years in each treatment arm
  LYs<-c(1,1,1,0)
  LYs.AZT<-trace.AZT%*%LYs
  #LYs.AZT
  LYs.comb<-trace.comb%*%LYs
  #LYs.comb

  #  Calculate costs and discounted costs in each treatment arm
  
  cost.AZT<-trace.AZT%*%dmc+trace.AZT%*%ccc+trace.AZT%*%azt
  #cost.AZT
  
  cost.comb<-trace.comb%*%dmc+trace.comb%*%ccc+trace.comb%*%azt
  cost.comb[1,1]<-cost.comb[1,1]+(trace.comb[1,1]+trace.comb[1,2]+trace.comb[1,3])*cLam
  cost.comb[2,1]<-cost.comb[2,1]+(trace.comb[2,1]+trace.comb[2,2]+trace.comb[2,3])*cLam
  #cost.comb
  
  ### undiscounted
  all.output.undis <- cbind(trace.AZT, LYs.AZT,cost.AZT,
        trace.comb, LYs.comb, cost.comb)
  colnames(all.output.undis) <- c("A.AZT","B.AZT","C.AZT","D.AZT",
                                  "LY.AZT","cost.AZT",
                                  "A.comb","B.comb","C.comb","D.comb",
                                  "LY.comb","cost.comb")
  
  ## need to add calculations and discounted cols for them to see
