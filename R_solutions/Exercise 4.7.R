#  DM4HEE 
#  Exercise 4.7 - Making the HIV/AIDS model probabilistic
#  Author: Andrew Briggs
#  Edited by: Nichola Naylor & Jack Williams
#  Date created: 21 February 2021
#  Date last edit: 29 March 2021

####****PARAMETERS****#####
#  Start by defining parameters
dr.c <- 0.06 ## discount rate for costs 
dr.o <- 0 ## discount rate for outcomes

#  Set the total number of cycles for the model to run
cycles <- 20 ## i.e. we want to run the model for each year for 20 years

state.names<-c("A.AsympHIV","B.SympHIV","C.AIDS","D.Death") 
## the ordering is important here, you will see why as we go 
## but this is stating the first value in the vector is "A.AsympHIV"
state.names

# Seed the starting states of the model
seed <- c(1,0,0,0) ## i.e. everyone starts in State A

## The treatment effect
RR.deterministic <- 0.509 ## The deterministic treatment effect (RR)
lnRR <- log(RR.deterministic) ## log mean treatment effect
se.lnRR <-(log(0.710)-log(0.365))/(2*1.96) ## log standard error of the treatment effect (estimated from confidence intervals)
RR <- exp(rnorm(1,lnRR,se.lnRR)) ## A random draw of RR based on the log mean and log standard error 
RR

### COSTS ####################

# mean values 
mn.dmca <- 1701  ## Mean direct medical costs associated with state A
mn.dmcb <- 1774  ## Mean direct medical costs associated with state B
mn.dmcc <- 6948  ## Mean direct medical costs associated with stateC

mn.ccca <- 1055 ## Mean Community care costs associated with state A
mn.cccb <- 1278 ## Mean Community care costs associated with state B
mn.cccc <- 2059 ## Mean Community care costs associated with state C

# standard error values
se.dmca <- mn.dmca ## Standard error for direct medical costs associated with state A
se.dmcb <- mn.dmcb  ## Standard error for direct medical costs associated with state B
se.dmcc <- mn.dmcc  ## Standard error for direct medical costs associated with stateC

se.ccca <- mn.ccca ## Mean Community care costs associated with state A
se.cccb <- mn.cccb ## Mean Community care costs associated with state B
se.cccc <- mn.cccc ## Mean Community care costs associated with state C

# alpha and beta values
a.dmca<-(mn.dmca/se.dmca)^2 ## alpha value for direct medical costs associated with state A
b.dmca<-(se.dmca^2)/mn.dmca ## beta value for direct medical costs associated with state A

a.dmcb<-(mn.dmcb/se.dmcb)^2 ## alpha value for direct medical costs associated with state B
b.dmcb<-(se.dmcb^2)/mn.dmcb ## beta value for direct medical costs associated with state B

a.dmcc<-(mn.dmcc/se.dmcc)^2
b.dmcc<-(se.dmcc^2)/mn.dmcc

a.ccca<-(mn.ccca/se.ccca)^2 ## alpha value for community care costs associated with state A
b.ccca<-(se.ccca^2)/mn.ccca ## alpha value for community care costs associated with state A

a.cccb<-(mn.cccb/se.cccb)^2
b.cccb<-(se.cccb^2)/mn.cccb

a.cccc<-(mn.cccc/se.cccc)^2
b.cccc<-(se.cccc^2)/mn.cccc

# random draw for costs based on the above & a Gamma distribution
c.dmca <- rgamma(1,shape=a.dmca,scale=b.dmca)  ## Probablistic value for direct medical costs associated with state A
c.dmcb <- rgamma(1,shape=a.dmcb,scale=b.dmcb) ## Probablistic value for medical costs associated with state B
c.dmcc <- rgamma(1,shape=a.dmcc,scale=b.dmcc)  ## Probablistic value for medical costs associated with stateC
c.dmc <- c(c.dmca, c.dmcb, c.dmcc,0) ## A vector storing the direct costs associated with each state
  
c.ccca <- rgamma(1,shape=a.ccca,scale=b.ccca) ## Probablistic value for Community care costs associated with state A
c.cccb <- rgamma(1,shape=a.cccb,scale=b.cccb) ## Probablistic value for Community care costs associated with state B
c.cccc <- rgamma(1,shape=a.cccc,scale=b.cccc) ## Probablistic value for Community care costs associated with state C
c.ccc <- c(c.ccca, c.cccb, c.cccc, 0) ## A vector storing the community costs associated with each state

c.dmc
c.ccc

#  Drug costs
c.AZT <- 2278  ### Zidovudine drug cost
c.LAM <- 2086.5 ## Lamivudine drug cost 
c.azt <- c(c.AZT, c.AZT, c.AZT, 0) ## A vector of Lamivudine drug costs per state
## Notice how the final death state has £0 cost
c.lam <- c(c.LAM, c.LAM, c.LAM, 0) ## A vector of Lamivudine drug costs per state

### TRANSITION PROBABILITIES ############

## defining alpha and beta for transitions
alpha.A2A <- 1251 ## Counts of transitions from A to A (see Table 2.5)
alpha.A2B <- 350 ## Counts of transitions from A  to B
alpha.A2C <- 116 ## Counts of transitions from A to C
alpha.A2D <- 17 ## Counts of transitions from A to D
alpha.B2B <- 731 ## Counts of transitions from B to B
alpha.B2C <- 512
alpha.B2D <- 15
alpha.C2C <- 1312
alpha.C2D <- 437

A.sum <- 1734 ## total counts of transitions out of A
B.sum <- 1258 ## total counts of transitions out of B
C.sum <- 1749 ## total counts of transitions out of C  




## Dirichlet distribution transitions

# transitions out of A
gd.A2A<-rgamma(1,shape=alpha.A2A,scale=1) ### Random draws from a Gamma distribution for A2A : setting the shape parameter to alpha and the scale paremeter to 1
gd.A2B<-rgamma(1,shape=alpha.A2B,scale=1)
gd.A2C<-rgamma(1,shape=alpha.A2C,scale=1)
gd.A2D<-rgamma(1,shape=alpha.A2D,scale=1)
gd.A <- gd.A2A + gd.A2B + gd.A2C + gd.A2D ## Sum of draws for transitions from A

tp.A2A<-gd.A2A/gd.A ## transition probability for A to A 
tp.A2B<-gd.A2B/gd.A ## transition probability for A to B
tp.A2C<-gd.A2C/gd.A
tp.A2D<-gd.A2D/gd.A

# transitions out of B
gd.B2B<-rgamma(1,shape=alpha.B2B,scale=1) ### Random draws from a Gamma distribution for B2B transitions : setting the shape parameter to alpha and the scale paremeter to 1
gd.B2C<-rgamma(1,shape=alpha.B2C,scale=1)
gd.B2D<-rgamma(1,shape=alpha.B2D,scale=1)
gd.B <- gd.B2B + gd.B2C + gd.B2D ## Sum of draws for transitions from B

tp.B2B<-gd.B2B/gd.B ## transition probability for B to B
tp.B2C<-gd.B2C/gd.B  ## transition probability for B to C
tp.B2D<-gd.B2D/gd.B

## Beta distribution transitions (AIDS to Death)
tp.C2D <-rbeta(1,shape1=alpha.C2D,shape2=(C.sum-alpha.C2D)) ## using the Beta distribution to estimate the transition probability between C and D
tp.C2C <- 1-tp.C2D  ## those staying in state C 

####**** MARKOV MODEL ****######

#  Now create a transition matrix for the AZT arm
#  This shows the probability of transitioning from one state to another 
n.states <- length(state.names)
A.AsympHIV.AZT <- c(tp.A2A, tp.A2B, tp.A2C, tp.A2D) ## all of the transitions out of A in one vector, with each value corresponding to a transition to a different state (A, B, C, D)
B.SympHIV.AZT <- c(0, tp.B2B, tp.B2C, tp.B2D) 
C.AIDS.AZT <- c(0, 0, tp.C2C, tp.C2D)
D.Death <- c(0,0,0,1) ## as nobody transitions out of dead the transition probability of staying in dead, once in dead, is equal to 1

tm.AZT <- matrix(data=rbind(A.AsympHIV.AZT,B.SympHIV.AZT,C.AIDS.AZT,D.Death),nrow=n.states,ncol=n.states)
rownames(tm.AZT) <- state.names ## renaming the matrix row names
colnames(tm.AZT) <- state.names ## renaming the matrix column names
tm.AZT 

#  Create a trace for the AZT arm
#  This captures the number of people in each state at any one time
trace.AZT <- matrix(data=NA, nrow=cycles, ncol=n.states) ## the length of the matrix is equivalent to the number of cycles
colnames(trace.AZT) <- state.names

## set the first row as the seed population (cycle0) multiplied by the transition matrix
# i.e. running the first cycle of the model
# Note this does not include any cost/effect of anything occuring before cycle 1
trace.AZT[1,] <- seed %*% tm.AZT 

## Let's see what the first few rows of the Markov trace looks like:
head(trace.AZT) ## the head() function returns the first 6 rows of a matrix (or data.frame)

## building the trace function requires you to loop through each row
## and multiply the number of people in A at time t-1 with transition probabilities related to A
## to get the number of people in different states at time t

for (i in 2:cycles) {   ### we want to get the estimates for cycle2 (row 3) to cycle20 (row 21)
  trace.AZT[i,] <- trace.AZT[i-1,] %*% tm.AZT
}

rownames(trace.AZT) <- paste("cycle", 1:cycles, sep = "_") ## assigning the rownames to highlight each row is 1 cycle run of the markov model
trace.AZT

rowSums(trace.AZT) ## check that they sum to 1, if not something has gone wrong in your calculations

##### LIFE YEARS #####
LY <-c(1,1,1,0) ## the reward vector (i.e. benefit from being in each state)

ly.AZT <- trace.AZT %*% LY 
ly.AZT

undisc.ly.AZT <- colSums(ly.AZT) ## calculating the total LYs from AZT arm
undisc.ly.AZT 

discount.factor.o <- matrix(1/(1+dr.o) ^ c(1:cycles), nrow = 1, ncol = cycles) ## discount factor matrix for outcomes
discount.factor.o

disc.ly.AZT <- discount.factor.o %*% ly.AZT ##multiply the discount factor matrix and the outcome matrix of the AZT arm
disc.ly.AZT

#### COST CALCULATIONS #####
## undiscounted:
cost.AZT <- trace.AZT %*% c.dmc + trace.AZT %*% c.ccc + trace.AZT %*% c.azt ## multply the matrix by each cost type and sum
cost.AZT

undisc.cost.AZT<-colSums(cost.AZT) ## calculating the total cost of the AZT arm
undisc.cost.AZT

discount.factor.c <- matrix(1/(1+dr.c) ^ c(1:cycles), nrow = 1, ncol = cycles) ## create a discount factor matrix
discount.factor.c

disc.cost.AZT <- discount.factor.c %*% cost.AZT ## calculating the total discounted cost of the AZT arm
disc.cost.AZT


#### COMBINATION THERAPY ARM ##### 
# Create a transition matrix for the combination therapy arm
A.AsympHIV.comb <- c(1-(tp.A2B+tp.A2C+tp.A2D)*RR,tp.A2B*RR,tp.A2C*RR,tp.A2D*RR)
B.SympHIV.comb <- c(0,1-(tp.B2C+tp.B2D)*RR,tp.B2C*RR,tp.B2D*RR)
C.AIDS.comb <- c(0,0,1-tp.C2D*RR,tp.C2D*RR)
## D.Death is the same as before so does not need redefining

tm.comb <- matrix(data=rbind(A.AsympHIV.comb,B.SympHIV.comb,C.AIDS.comb,D.Death),nrow=n.states,ncol=n.states)
rownames(tm.comb) <- state.names
colnames(tm.comb) <- state.names
tm.comb

#  Create a trace for the combination arm
trace.comb <- matrix(data=NA,nrow=cycles,ncol=n.states)
colnames(trace.comb) <- state.names

## utilising the new matrix for the first 2 years (when combination therapy is given):
trace.comb[1,] <- seed%*%tm.comb
trace.comb[2,] <- trace.comb[1,]%*%tm.comb

## then reverting back to the previous matrix:
for (i in 3:cycles) {
  trace.comb[i,] <- trace.comb[i-1,]%*%tm.AZT
}
trace.comb

ly.comb <- trace.comb%*%LY
ly.comb

undisc.ly.comb <- colSums(ly.comb)
undisc.ly.comb

disc.ly.comb <- discount.factor.o%*%ly.comb 
disc.ly.comb

cost.comb<-trace.comb%*%c.dmc+trace.comb%*%c.ccc+trace.comb%*%c.azt
## we need to replace rows 1 and 2 to add in the cost of lamivudine for years 1 and 2
cost.comb[1,1] <- cost.comb[1,1] + (trace.comb[1,1] + trace.comb[1,2] + trace.comb[1,3]) *c.LAM
cost.comb[2,1] <- cost.comb[2,1] + (trace.comb[2,1] + trace.comb[2,2] + trace.comb[2,3]) *c.LAM
cost.comb

undisc.cost.comb <- colSums(ly.AZT) 
undisc.ly.AZT 

disc.cost.comb <- discount.factor.c %*% cost.comb 
disc.cost.comb

#######**** ANALYSIS *****#####
#  Cost-effectiveness results

### output table
output <- c(inc.cost = disc.cost.comb - disc.cost.AZT,
            inc.lys = disc.ly.comb - disc.ly.AZT,
            icer = NA)
output[3] <- output[1]/output[2]
round(output, 2) ## printing rounded version of the output table