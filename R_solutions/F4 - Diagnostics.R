## F4 Diagnostics 



#### Model Parameters #### 

lambda <- 30000 # This is our willingness to pay threshold
prevalence <- 0.3

# Payoffs and cost parameters for sick and healthy individuals, treated and not treated 

outcome.names <- c("Sick per treated", "Sick person not treated", 
                   "Healthy person treated", "Health person not treated")
expected.cost <- c(6000, 5000, 3000, 1000)
expected.qaly <- c(0.8, 0.5, 0.95, 1)

parameter.values <- data.frame(outcome.names, expected.cost, expected.qaly,
                              nmb = NA)

parameter.values$nmb <- (expected.qaly * lambda) - expected.cost

# here you can see the expected costs, QALYs and NMB for each possible outcome
parameter.values


# Estimating costs and QALYs associated with treating all vs. treat none 

treat.cost <- prevalence * expected.cost[1] + (1 - prevalence) * expected.cost[3]
notreat.cost <- prevalence * expected.cost[2] + (1 - prevalence) * expected.cost[4]
treat.qaly <- prevalence * expected.qaly[1] + (1 - prevalence) * expected.qaly[3]
notreat.qaly <- prevalence * expected.qaly[2] + (1 - prevalence) * expected.qaly[4]

expected.values <- data.frame(strategy =  c("Treat all", "Treat none"),
                           expected.cost = c(treat.cost, notreat.cost), 
                           expected.qaly = c(treat.qaly, notreat.qaly), 
                           nmb = NA)

expected.values$nmb <- (expected.values$expected.qaly * lambda) - expected.values$expected.cost 

# You can view the expected cost, QALY and NMB values at the prevalence defined
expected.values



# Evaluate NMB across a range of prevalence values

## We can create a function to calcualte NMB for sick and healthy patients (across prevalence values)  
est.nmb <- function(prev, parameters = parameter.values, lam = lambda){
  
  # Treatment - look up correct costs and outcomes for treated

  cost.sick <-  parameters$expected.cost[1] 
  cost.healthy <-  parameters$expected.cost[3] 
  
  qaly.sick <-  parameters$expected.qaly[1] 
  qaly.healthy <-  parameters$expected.qaly[3] 
  
  cost <- prev * cost.sick + (1-prev) * cost.healthy
  qaly <- prev * qaly.sick + (1-prev) * qaly.healthy
  
  nmb.treat <- (qaly * lam) - cost
  
  # No treatment- look up correct costs and outcomes for not treated

  cost.sick <-  parameters$expected.cost[2] 
  cost.healthy <-  parameters$expected.cost[4] 
  
  qaly.sick <-  parameters$expected.qaly[2] 
  qaly.healthy <-  parameters$expected.qaly[4] 
  
  cost <- prev * cost.sick + (1-prev) * cost.healthy
  qaly <- prev * qaly.sick + (1-prev) * qaly.healthy
  
  nmb.notreat <- (qaly * lam) - cost
  
  return(data.frame(prevalence = prev, 
                    nmb.treat = nmb.treat, 
                    nmb.notreat = nmb.notreat))
}


## We can now estimate the NMB of treating or not treating at any prevalence 
# At a low prevalence (10%), NMB is higher for not treating
# But at higher prevalence NMB is higher for treating everyone
est.nmb(prev = 0.1)
est.nmb(prev = 0.4)

# Now pass a vector of prevalence values into the function
prevalence.vector <- seq(from = 0, to = 1, by = 0.05)

# estimate the NMB across prevalence values, and save this 
no.test.nmb <- est.nmb(prev = prevalence.vector)
no.test.nmb


# We can create a plot in base R to observe the results
# Note - we will be updating this plot in the next section
plot(no.test.nmb$prevalence, no.test.nmb$nmb.treat, type="l", col = "blue", ylim = c(0, 30000))
  lines(no.test.nmb$prevalence, no.test.nmb$nmb.notreat, col = "red")
  legend(0.7, 30000, legend=c("Treat all", "Treat none"), col=c("blue", "red"), 
         lty = 1, cex = 0.8)
  


#### Expected value of a perfect test ####

# In this example, we will assume that all sick patients are treated, and all healthy patients are not


est.nmb.perfect.test <- function(prev, parameters = parameter.values, lam = lambda){
  
  # Perfect test means sick is treated (i.e. row value 1 for expected values table)
  # whilst healthy is not treated (i.e. row value 4 from expected values table)
  # So we eliminate sick person not treated (row 2), and health person treated (row 3)
  
  cost.sick <-  parameters$expected.cost[1] 
  cost.healthy <-  parameters$expected.cost[4] 
  
  qaly.sick <-  parameters$expected.qaly[1] 
  qaly.healthy <-  parameters$expected.qaly[4] 
  
  cost <- prev * cost.sick + (1-prev) * cost.healthy
  qaly <- prev * qaly.sick + (1-prev) * qaly.healthy
  
  nmb.perfect <- (qaly * lam) - cost
  

  return(data.frame(prevalence = prev, 
                    nmb = nmb.perfect))
}

est.nmb.perfect.test(prev = 0.1)
est.nmb.perfect.test(prev = prevalence.vector)

perfect.test.nmb <- est.nmb.perfect.test(prev = prevalence.vector)


# Estimate the Expected Value of Perfect Diagnostic Information (EVPDI) 

# The EVPDI is the difference between the expected value of a perfect test and 
# the expected value of the optimal course of action (treat All or treat None) in the absence of a test.

# First, look again at the results of the NMB with no test available 
no.test.nmb

# Now we can use the apply function to derive the maximum NMB from each strategy, and add to current table
no.test.nmb$nmb.max <- apply(no.test.nmb[,2:3], 1, max)

## Now take a look at the current table, displaying the maximum NMB for each prevalence (without a test)
# We can compare this to the NMB with a perfect test

no.test.nmb
perfect.test.nmb


## We can also plot the results of the these tables

plot(no.test.nmb$prevalence, no.test.nmb$nmb.treat, type="l", col = "blue", ylim = c(0, 30000))
lines(no.test.nmb$prevalence, no.test.nmb$nmb.notreat, col = "red")
lines(perfect.test.nmb$prevalence, perfect.test.nmb$nmb, col = "dark green")
legend(0.7, 30000, legend=c("Treat all", "Treat none", "Perfect test"), col=c("blue", "red", "dark green"), 
       lty = 1, cex = 0.8)




## We can now compare this to the values expected from a perfect test
evpdi <- perfect.test.nmb$nmb - no.test.nmb$nmb.max

# and create a data.frame to store the results
evpdi.table <- data.frame(prevalence = prevalence.vector, evpdi = evpdi)
evpdi.table

# We can create a simple plot of the expected value of perfect diagnostic information, by prevalence:
plot(evpdi.table, type="l", col = "blue", ylim = c(0, 3000))



#### Expected Value of Clinical Information (EVCI) from an (imperfect) diagnostic test ####

test.char <- c(positive.mean = 1, positive.sd = 1, 
               negative.mean = -1, negative.sd = 1)

# You can see the test characteristics here
test.char


# Set a range of diagnostic threshold values
diagnostic.threshold <- seq(from = -4, to = 4, by = 0.25)

# Diagnostic positive and negative values, at different diagnostic thresholds
# These are estimated using the normal distribution 
diagnostic.pos <- prevalence * dnorm(diagnostic.threshold, mean = test.char[1], sd = test.char[2])
diagnostic.neg <- (1 - prevalence) * dnorm(diagnostic.threshold, mean = test.char[3], sd = test.char[4])

biomarker.dist <- data.frame(diagnostic.threshold, diagnostic.pos, diagnostic.neg)

# We can view the table, and round to 4 decimal places to help view the numbers
round(biomarker.dist, 4)

# And here we can plot the numbers to re-create the video 
# (note we can use the data frame columns (biomarker.dist), or the vectors that formed the data frame)
plot(diagnostic.threshold, diagnostic.pos, type="l", col = "blue", ylim = c(0, 0.3))
lines(diagnostic.threshold, diagnostic.neg, col = "red")


## True Positive Rate and False Positive Rate 
TPR <- 1 - pnorm(diagnostic.threshold, test.char[1], test.char[2])
FPR <- 1 - pnorm(diagnostic.threshold, test.char[3], test.char[4])

# We can view these alongside the diagnostic thresholds 
positive.rate <- data.frame(diagnostic.threshold, TPR, FPR)


# Plot to view TPR and FPR (Receiver Operating Characteristic (ROC) curve)
plot(FPR, TPR, type="l", col = "red")
# and we can add a diagonal line too
lines(c(0,1),(c(0,1)), col = "grey")


#### Expected Value of Clinical Information #### 

# We can now estimate the number of true and false, negatives and positives 
true.pos <- prevalence * TPR 
false.neg <- prevalence * (1 - TPR) 
false.pos <- (1 - prevalence) * FPR 
true.neg <- (1 - prevalence) * (1 - FPR) 

# Table showing diagnostic accuracy of the imperfect test
diag.accuracy <- data.frame(true.pos, false.neg, false.pos, true.neg,
                            nmb.test = 0)

# Now combine the original NMB values (from parameter data frame) to estimate NMB as 
# a function of the diagnostic accuracy 
parameter.values$nmb 

# To combine the NMB payoff values with the diagnostic accuracy data, we first create a blank 
# vector to store the total NMB value at each diagnostic threshold
nmb.test.vec <- rep(0, length(diagnostic.threshold))

# Next, we can create a loop to multiply NMB payoffs with diagnostic accuracy, and save the sum of
# these values, at each diagnostic threshold
for(i in 1:length(diagnostic.threshold)){
  nmb.test.vec[i] <- sum(diag.accuracy[i,] * parameter.values$nmb)
}  

## Note: An alternative method is to write a function into apply      
nmb.test.vec <- apply(diag.accuracy, 1, function(x) sum(x * parameter.values$nmb))


# We can view the output of the calculation here
# This is the NMB associated with the imperfect diagnostic test
data.frame(diagnostic.threshold, nmb.test.vec)    

# We can add these results onto the diagnostic accuracy data.frame 
diag.accuracy$nmb <- nmb.test.vec


# Calculate the EVCI

# Now we want to calcualte the EVCI across a range of prevalence values
no.test.nmb <- est.nmb(prev = prevalence.vector)


# The est.evci function below calculates the diagnostic accuracy and associated NMB 
# of an imperfect test (similar to calculations above) and also estimates the NMB 
# in the absense of any test, to estimate the Expected Value of Clinical Information 
# The function also does this at different prevalence values (by passing the prevalence)
# to the function (indicated by 'prev' argument)

est.evci <- function(prev, diag = diagnostic.threshold, test = test.char, 
                     nmb.input = parameter.values$nmb, nmb.notest = no.test.nmb){

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
  
  nmb.notest <- est.nmb(prev = prev)
  
  max.nmb.notest <- apply(nmb.notest[,2:3], 1, max) # this derives the maximum value for each row (prevalence by rows in this table)
  
  evci <- data.frame(prev, 
                     nmb.notest = max.nmb.notest, 
                     nmb.imperfect.test = max.nmb.test,
                     evci = max.nmb.test - max.nmb.notest)
  
  return(evci)
    
}


# Try the est.EVCI at the 30% prevalence 
est.evci(prev = 0.3)


# Save the EVCI results across a range of prevalence
evci <- est.evci(prev = prevalence.vector)

# You can view the results using the round function 
round(evci, 2)


# We can create a plot of the EVCI compared to the EVDI calculated earlier

plot(evpdi.table, type="l", col = "blue", ylim = c(0, 3000))
lines(evci$prev, evci$evci, col = "red")
legend("topright", legend=c("EVDI", "EVCI"), col=c("blue", "red"), 
       lty = 1, cex = 0.8)





## ggplots (if time) 

## Jack to add ggplots code and do a facet wrap to show them all... 
