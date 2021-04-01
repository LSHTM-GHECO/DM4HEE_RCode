#  DM4HEE 
#  Exercise 4.7 - Making the HIV/AIDS model probabilistic
#  Author: Andrew Briggs
#  Edited by: Nichola Naylor & Jack Williams
#  Date created: 22 February 2021
#  Date last edit: 1 April  2021


# In this example, the HIV model is wrapped up in a function - so that the function can run the whole model script
# The model code is the same as that in 4.7
# Once the model is in the function, the function can be called to easily produce the model results 
# We can then create a loop to perform a large number of model simulations, for the PSA 



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


# Here we create the function to run the model 
# Note 1 - the printing / displaying of data as we go through the code has been taken out (to speed up simulations later)   
# Note 2 - this is a simplistic way to create the model - 
#             - It is easy because everything is within the function, but.... 
#             - when running the model lots of times, repeatedly assigning fixed values can mean the simulations are slow
#             - we will look at alternatives in later exercises


#### Model function ##### 

model.HIV <- function() {
  
  ## The treatment effect
  RR.deterministic <- 0.509 ## The deterministic treatment effect (RR)
  lnRR <- log(RR.deterministic) ## log mean treatment effect
  se.lnRR <-(log(0.710)-log(0.365))/(2*1.96) ## log standard error of the treatment effect (estimated from confidence intervals)
  RR <- exp(rnorm(1,lnRR,se.lnRR)) ## A random draw of RR based on the log mean and log standard error 
  
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
  
  head(trace.AZT)
  
  ## building the trace function requires you to loop through each row
  ## and multiply the number of people in A at time t-1 with transition probabilities related to A
  ## to get the number of people in different states at time t
  
  for (i in 2:cycles) {   ### we want to get the estimates for cycle2 (row 3) to cycle20 (row 21)
    trace.AZT[i,] <- trace.AZT[i-1,] %*% tm.AZT
  }
  
  rownames(trace.AZT) <- paste("cycle", 1:cycles, sep = "_") ## assigning the rownames to highlight each row is 1 cycle run of the markov model

  ##### LIFE YEARS #####
  LY <-c(1,1,1,0) ## the reward vector (i.e. benefit from being in each state)
  
  ly.AZT <- trace.AZT %*% LY 
  
  undisc.ly.AZT <- colSums(ly.AZT) ## calculating the total LYs from AZT arm
  
  discount.factor.o <- matrix(1/(1+dr.o) ^ c(1:cycles), nrow = 1, ncol = cycles) ## discount factor matrix for outcomes
  
  disc.ly.AZT <- discount.factor.o %*% ly.AZT ##multiply the discount factor matrix and the outcome matrix of the AZT arm
  
  
  #### COST CALCULATIONS #####
  ## undiscounted:
  cost.AZT <- trace.AZT %*% c.dmc + trace.AZT %*% c.ccc + trace.AZT %*% c.azt ## multply the matrix by each cost type and sum

  undisc.cost.AZT<-colSums(cost.AZT) ## calculating the total cost of the AZT arm

  discount.factor.c <- matrix(1/(1+dr.c) ^ c(1:cycles), nrow = 1, ncol = cycles) ## create a discount factor matrix

  disc.cost.AZT <- discount.factor.c %*% cost.AZT ## calculating the total discounted cost of the AZT arm

  
  #### COMBINATION THERAPY ARM ##### 
  # Create a transition matrix for the combination therapy arm
  A.AsympHIV.comb <- c(1-(tp.A2B+tp.A2C+tp.A2D)*RR,tp.A2B*RR,tp.A2C*RR,tp.A2D*RR)
  B.SympHIV.comb <- c(0,1-(tp.B2C+tp.B2D)*RR,tp.B2C*RR,tp.B2D*RR)
  C.AIDS.comb <- c(0,0,1-tp.C2D*RR,tp.C2D*RR)
  ## D.Death is the same as before so does not need redefining
  
  tm.comb <- matrix(data=rbind(A.AsympHIV.comb,B.SympHIV.comb,C.AIDS.comb,D.Death),nrow=n.states,ncol=n.states)
  rownames(tm.comb) <- state.names
  colnames(tm.comb) <- state.names

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

  ly.comb <- trace.comb%*%LY

  undisc.ly.comb <- colSums(ly.comb)

  disc.ly.comb <- discount.factor.o%*%ly.comb 

  cost.comb<-trace.comb%*%c.dmc+trace.comb%*%c.ccc+trace.comb%*%c.azt
  ## we need to replace rows 1 and 2 to add in the cost of lamivudine for years 1 and 2
  cost.comb[1,1] <- cost.comb[1,1] + (trace.comb[1,1] + trace.comb[1,2] + trace.comb[1,3]) *c.LAM
  cost.comb[2,1] <- cost.comb[2,1] + (trace.comb[2,1] + trace.comb[2,2] + trace.comb[2,3]) *c.LAM

  undisc.cost.comb <- colSums(ly.AZT) 

  disc.cost.comb <- discount.factor.c %*% cost.comb 

  #######**** ANALYSIS *****#####

  ### output table (for simulations, only want incremental costs and outcomes)
  output <- c(inc.cost = disc.cost.comb - disc.cost.AZT,
              inc.lys = disc.ly.comb - disc.ly.AZT)
  
  return(output) ## this is what the function returns (i.e. the output of the function) 
  
}


# You can run the function to get the results of a simulation (each simulation will provide different results!)
model.HIV()


#### Probabilistic model runs #### 

# Now we know we can generate model results using a function, we can run and store a number of simulations

# First, we set the number of simulations to run  
sim.runs <- 1000

# We can create a data frame to store results
simulation.results <- data.frame(matrix(0, nrow = sim.runs, ncol = 2))
colnames(simulation.results) <- c("inc.LYs","inc.costs")

#### Running simulations & recording results #### 

# Now Loop through the appropriate number of simulations and store the function results into the results data.frame 
for(i in 1:sim.runs) simulation.results[i,] <- model.HIV() 

# We can check that the results are storing correctly 
head(simulation.results)

# And can return the mean results across all simulations 
colMeans(simulation.results)

#### Plots #####

# Cost-effectiveness plane (using base R to produce the plot)
plot(simulation.results$inc.LYs,simulation.results$inc.cost)



## Since 4.7 is exactly the same, maybe they can see how the trace works there??
## If that's ok, I can delete the below



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
