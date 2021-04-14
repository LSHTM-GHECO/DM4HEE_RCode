## F4 Diagnostics 




#### Model Parameters #### 

lambda <- 30000 # This is our willingness to pay threshold
prevalence <- 0.3

# payoffs and consequences 

outcome.names <- c("Sick per treated", "Sick person not treated", 
                   "Healthy person treated", "Health person not treated")

expected.cost <- c(6000, 5000, 3000, 1000)
expected.QALY <- c(0.8, 0.5, 0.95, 1)

expected.values <- data.frame(outcome.names, expected.cost, expected.QALY,
                              NMB = NA)

expected.values$NMB <- (expected.QALY * lambda) - expected.cost

# here you can see the expected costs, QALYs and NMB for each possible outcome
expected.values


# Estimating costs and QALYs associated with treating all vs. treat none 

treat.cost <- prevalence * expected.cost[1] + (1 - prevalence) * expected.cost[3]
notreat.cost <- prevalence * expected.cost[2] + (1 - prevalence) * expected.cost[4]

treat.QALY <- prevalence * expected.QALY[1] + (1 - prevalence) * expected.QALY[3]
notreat.QALY <- prevalence * expected.QALY[2] + (1 - prevalence) * expected.QALY[4]

treat.payoff <- data.frame(strategy =  c("Treat all", "Treat none"),
                           expected.cost = c(treat.cost, notreat.cost), 
                           expected.QALY = c(treat.QALY, notreat.QALY), 
                           NMB = NA)

treat.payoff$NMB <- (treat.payoff$expected.QALY * lambda) - treat.payoff$expected.cost 

treat.payoff



## Table 3 / 

# Evaluate NMB across a range of prevalence values

## We can create a function to calcualte NMB from the costs and QALYs of sick and healthy patients with prevalence  
est.NMB <- function(prev, parameters = expected.values, lam = lambda){
  
  # Treatment - look up correct costs and outcomes for treated

  cost.sick <-  parameters$expected.cost[1] 
  cost.healthy <-  parameters$expected.cost[3] 
  
  qaly.sick <-  parameters$expected.QALY[1] 
  qaly.healthy <-  parameters$expected.QALY[3] 
  
  cost <- prev * cost.sick + (1-prev) * cost.healthy
  qaly <- prev * qaly.sick + (1-prev) * qaly.healthy
  
  nmb.treat <- (qaly * lam) - cost
  
  # No treatment- look up correct costs and outcomes for not treated

  cost.sick <-  parameters$expected.cost[2] 
  cost.healthy <-  parameters$expected.cost[4] 
  
  qaly.sick <-  parameters$expected.QALY[2] 
  qaly.healthy <-  parameters$expected.QALY[4] 
  
  cost <- prev * cost.sick + (1-prev) * cost.healthy
  qaly <- prev * qaly.sick + (1-prev) * qaly.healthy
  
  nmb.notreat <- (qaly * lam) - cost
  
  return(data.frame(prevalence = prev, 
                    NMB.treat = nmb.treat, 
                    NMB.notreat = nmb.notreat))
}


## We can now estimate the NMB of treating or not treating at any prevalence
est.NMB(prev = 0.1)

# At a low prevalence (10%), NMB is higher for not treating
# But at higher prevalence NMB is higher for treating everyone
est.NMB(prev = 0.4)


# We can also pass a vector of prevalence values into the function

prevalence.vector <- seq(from = 0, to = 1, by = 0.05)
est.NMB(prev = prevalence.vector)

## Expected value of a perfect test

# In this example, we will assume that all sick patients are treated, and all healthy patients are not


est.NMB.perfect.test <- function(prev, parameters = expected.values, lam = lambda){
  
  # Perfect test means sick is treated (i.e. row value 1 for expected values table)
  # whilst healthy is not treated (i.e. row value 4 from expected values table)
  # So we eliminate sick person not treated (row 2), and health person treated (row 3)
  
  cost.sick <-  parameters$expected.cost[1] 
  cost.healthy <-  parameters$expected.cost[4] 
  
  qaly.sick <-  parameters$expected.QALY[1] 
  qaly.healthy <-  parameters$expected.QALY[4] 
  
  cost <- prev * cost.sick + (1-prev) * cost.healthy
  qaly <- prev * qaly.sick + (1-prev) * qaly.healthy
  
  nmb.perfect <- (qaly * lam) - cost
  

  return(data.frame(prevalence = prev, 
                    NMB.perfect.test = nmb.perfect))
}

est.NMB.perfect.test(prev = 0.1)
est.NMB.perfect.test(prev = prevalence.vector)


### Expected value of perfect diagnostic information (EVPDI) 

# The EVPDI is the difference between the expected value of a perfect test and 
# the expected value of the optimal course of action (treat All or treat None) in the absence of a test.



## We will do this for all prevalence values



perfect.test <- est.NMB.perfect.test(prev = prevalence.vector)

# First, look again at the results of the NMB with no test available (we can call this current)
current <- est.NMB(prev = prevalence.vector)

# now we can use the apply function to derive the maximum NMB from each strategy, and add to current table
current$NMB.max <- apply(current[,2:3], 1, max)

## Now take a look at the current table, displaying the maximum NMB for each prevalence (without a test)
current


## We can now compare this to the values expected from a perfect test
evpdi <- perfect.test$NMB.perfect.test - current$NMB.max

evdpi.table <- data.frame(prevalence = prevalence.vector, 
                          evpdi = evpdi)

evdpi.table


### Expected Value of Clinical Information (EVCI) from an (imperfect) diagnostic test

test.char <- c(positive.mean = 1, positive.sd = 1, 
               negative.mean = -1, negative.sd = 1)

test.char

diagnostic.threshold <- seq(from = -4, to = 4, by = 0.25)


diagnostic.pos <- prevalence * dnorm(diagnostic.threshold, mean = test.char[1], sd = test.char[2])
diagnostic.neg <- (1 - prevalence) * dnorm(diagnostic.threshold, mean = test.char[3], sd = test.char[4])

biomarker.dist <- data.frame(diagnostic.threshold, diagnostic.pos, diagnostic.neg)

# We can view the table, and round to 4 decimal places to help view the numbers
round(biomarker.dist, 4)

## TPR and FPR 

TPR <- 1 - pnorm(diagnostic.threshold, test.char[1], test.char[2])
FPR <- 1 - pnorm(diagnostic.threshold, test.char[3], test.char[4])

# We can view these alongside the diagnostic thresholds 
data.frame(diagnostic.threshold, TPR, FPR)

true.pos <- prevalence * TPR 
false.neg <- prevalence * (1 - TPR) 
false.pos <- (1 - prevalence) * FPR 
true.neg <- (1 - prevalence) * (1 - FPR) 

# Table showing diagnostic accuracy 
diag.accuracy <- data.frame(diagnostic.threshold, true.pos, false.neg, false.pos, true.neg,
                            NMB.test = 0)


# Estimate the NMB as a function of the diagnostic accuracy 

NMB.test.vec <- rep(0, length(diagnostic.threshold))

for(i in 1:length(diagnostic.threshold)){
  NMB.test.vec[i] <- sum(diag.accuracy[i,2:5] * expected.values$NMB)
}  

## Note: An alternative method is to write a function into apply      
NMB.test.vec <- apply(diag.accuracy[,2:5], 1, function(x) sum(x * expected.values$NMB))


# We can view the NMB here
data.frame(diagnostic.threshold, NMB.test.vec)    



diag.accuracy$NMB <- NMB.test


# Calculate the EVCI (?)

# Now we want to calcualte the EVCI across a range of prevalence values

nmb.no.test <- est.NMB(prev = prevalence.vector)


est.EVCI <- function(prev, diag = diagnostic.threshold, test = test.char, 
                     nmb.input = expected.values$NMB, nmb.notest = nmb.no.test){

  # true and false positive rate
  TPR <- 1 - pnorm(diag, test[1], test[2])
  FPR <- 1 - pnorm(diag, test[3], test[4])
  
  # calculate nmb for diagnostic outcomes
  
  prev.matrix <- matrix(prev, nrow = length(TPR), ncol = length(prev), byrow = TRUE)
  colnames(prev.matrix) <- prev
  rownames(prev.matrix) <- diag
  
  true.pos.nmb <- TPR * nmb.input[1] * prev.matrix
  false.neg.nmb <- (1 - TPR)  * nmb.input[2] * prev.matrix
  false.pos.nmb <-  FPR * nmb.input[3]  * (1 - prev.matrix)
  true.neg.nmb <-  (1 - FPR) * nmb.input[4]  * (1 - prev.matrix)
    
  total.nmb.matrix <- true.pos.nmb + false.neg.nmb + false.pos.nmb + true.neg.nmb
  
  # get the max nmb for each prevalence value 
  
  max.nmb.test <- apply(total.nmb.matrix, 2, max) # this derives the maximum value from each column 
  
  nmb.notest <- est.NMB(prev = prev)
  
  max.nmb.notest <- apply(nmb.notest[,2:3], 1, max) # this derives the maximum value for each row (prevalence by rows in this table)
  
  EVSI <- data.frame(prev, EVSI = max.nmb.test - max.nmb.notest)
  
  return(EVSI)
    
}


est.EVCI(prev = prevalence.vector)

