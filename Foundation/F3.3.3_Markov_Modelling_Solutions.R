#  Decision Modelling for Health Economic Evaluation
#  Foundation Course Exercise 3: SOLUTION FILE
#  Authors: Andrew Briggs, Nichola Naylor and Jack Williams 

####****PARAMETERS****#####
#  Start by defining parameters
state.names<-c("A.AsympHIV","B.SympHIV","C.AIDS","D.Death") 
              ## the ordering is important here, you will see why as we go 
              ## but this is stating the first value in the vector is "A.AsympHIV"
state.names

### TRANSITION PROBABILITIES ############

alpha.A2A <- 1251 ## Counts of transitions from A to A
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

tp.A2A <- alpha.A2A / A.sum ## transition probability of A to A
tp.A2B <- alpha.A2B / A.sum ## transition probability of A to B
tp.A2C <- alpha.A2C / A.sum  
tp.A2D <- alpha.A2D / A.sum
tp.B2B <- alpha.B2B / B.sum ## transition probability of B to B
tp.B2C <- alpha.B2C / B.sum ## transition probability of B to C
tp.B2D <- alpha.B2D / B.sum
tp.C2C <- alpha.C2C / C.sum ## transition probability of C to C
tp.C2D <- alpha.C2D / C.sum ## transition probability of C to D

# ### Note you could input the numbers directly 
# ## e.g
# tp.A2A <- 1251/1734 
# ## but for understanding of where the numbers come from
# ## we asked you to work through the calculations of these numbers
# ## also note if we wanted to calculate complements we can do this using:
# beta.A2A <- A.sum-alpha.A2A

### COSTS ####################
c.dmca <- 1701  ## Direct medical costs associated with state A
c.dmcb <- 1774  ##Direct medical costs associated with state B
c.dmcc <- 6948  ## Direct medical costs associated with stateC
c.dmc <- c(c.dmca, c.dmcb, c.dmcc,0) ## A vector storing the direct costs associated with each state
## the order is important as these will be multiplied according to 
## matrix multiplication 
## (e.g. first value in dmc - direct medical cost of A - will be multiplied by 
## first value in a matrix that represents number of cases in state A) 
c.ccca <- 1055 ## Community care costs associated with state A
c.cccb <- 1278 ## Community care costs associated with state B
c.cccc <- 2059 ## Community care costs associated with state C
c.ccc <- c(c.ccca, c.cccb, c.cccc, 0) ## A vector storing the community costs associated with each state

#  Drug costs
c.AZT <- 2278  ### Zidovudine drug cost
c.LAM <- 2086.5 ## Lamivudine drug cost 
c.azt <- c(c.AZT, c.AZT, c.AZT, 0) ## A vector of AZT drug costs per state
## Notice how the final death state has £0 cost
c.lam <- c(c.LAM, c.LAM, c.LAM, 0) ## A vector of Lamivudine drug costs per state

### OTHER PARAMETERS #######
RR <- 0.509 ## Treatment effect (RR)
dr.c <- 0.035 ## Annual discount rate - costs (%)
dr.o <- 0.035  ## Annual discount rate - benefits (%) 

####**** MARKOV MODEL ****######
#  Set the total number of cycles for the model to run
cycles <- 20 ## i.e. we want to run the model for each year for 20 years

#  Seed the starting states of the model (cycle 0)
seed <- c(1,0,0,0) ## i.e. everyone starts in State A

#  Now create a transition matrix for the AZT arm
#  This shows the probability of transitioning from one state to another 
n.states <- length(state.names)
A.AsympHIV.AZT <- c(tp.A2A, tp.A2B, tp.A2C, tp.A2D) ## all of the transitions out of A in one vector, with each value corresponding to a transition to a different state (A, B, C, D)
B.SympHIV.AZT <- c(0, tp.B2B, tp.B2C, tp.B2D) 
C.AIDS.AZT <- c(0, 0, tp.C2C, tp.C2D)
D.Death <- c(0,0,0,1) ## as nobody transitions out of dead the transition probability of staying in dead, once in dead, is equal to 1
## as nobody transitions out of dead the transition probability of staying in dead, once in dead, is equal to 1

tm.AZT <- matrix(c(A.AsympHIV.AZT,B.SympHIV.AZT,C.AIDS.AZT,D.Death), 
                 nrow = n.states, ncol=n.states, byrow = TRUE)
rownames(tm.AZT) <- state.names ## renaming the matrix row names
colnames(tm.AZT) <- state.names ## renaming the matrix column names
tm.AZT 

#  Create a trace for the AZT arm
#  This captures the number of people in each state at any one time
trace.AZT <- matrix(data=NA, nrow=cycles, ncol=n.states) ## the length of the matrix is equivalent to the number of cycles
colnames(trace.AZT) <- state.names

## set the first row as the seed population (cycle0) multiplied by the transition matrix
# i.e. running the first cycle of the model
# Note this does not include any cost/effect of anything occurring before cycle 1
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


discount.factor.o <- matrix(data=NA, nrow=1, ncol=cycles)
for (i in 1:cycles) {
  discount.factor.o[1,i]<-1/(1+dr.o)^i
}
discount.factor.o

disc.ly.AZT <- discount.factor.o %*% ly.AZT ##multiply the discount factor matrix and the outcome matrix of the AZT arm
disc.ly.AZT


#### COST CALCULATIONS #####
## undiscounted:
cost.AZT <- trace.AZT %*% c.dmc + trace.AZT %*% c.ccc + trace.AZT %*% c.azt ## multiply the matrix by each cost type and sum
cost.AZT

undisc.cost.AZT<-colSums(cost.AZT) ## calculating the total cost of the AZT arm
undisc.cost.AZT

discount.factor.c<-matrix(data=NA, nrow=1, ncol=cycles)
for (i in 1:cycles) {
  discount.factor.c[1,i] <- 1/(1 + dr.c)^i
}
discount.factor.c

disc.cost.AZT <- discount.factor.c %*% cost.AZT ## calculating the total discounted cost of the AZT arm
disc.cost.AZT


#### COMBINATION THERAPY ARM ##### 

# Create a transition matrix for the combination therapy arm
A.AsympHIV.comb <- c(1-(tp.A2B+tp.A2C+tp.A2D)*RR,tp.A2B*RR,tp.A2C*RR,tp.A2D*RR)
B.SympHIV.comb <- c(0,1-(tp.B2C+tp.B2D)*RR,tp.B2C*RR,tp.B2D*RR)
C.AIDS.comb <- c(0,0,1-tp.C2D*RR,tp.C2D*RR)
## D.Death is the same as before so does not need redefining

tm.comb <- matrix(c(A.AsympHIV.comb, B.SympHIV.comb, C.AIDS.comb, D.Death), 
       nrow = n.states, ncol=n.states, byrow = TRUE)

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

undisc.cost.comb <- colSums(cost.comb) 
undisc.cost.comb

disc.cost.comb <- discount.factor.c %*% cost.comb 
disc.cost.comb

#######**** ANALYSIS *****#####
#  Cost-effectiveness results

### output table
output <- c(inc.cost = disc.cost.comb - disc.cost.AZT,
            inc.lys = disc.ly.comb - disc.ly.AZT,
            icer = NA)
output["icer"] <- output[1]/output[2]
output

### Note if you want to round these outputs using the round() functon:
round(output, 2)



#####****More Efficient Model Notes****#####
## More efficient code for discounting is defined below, there are many other ways to do this, you can check results against your chosen method
discount.factor.o <- matrix(1/(1+dr.o) ^ c(1:cycles), nrow = 1, ncol = cycles)
discount.factor.c <- matrix(1/(1+dr.c) ^ c(1:cycles), nrow = 1, ncol = cycles)

