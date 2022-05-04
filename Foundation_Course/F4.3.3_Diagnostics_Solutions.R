#  Decision Modelling for Health Economic Evaluation
#  Foundation Course Exercise 4: SOLUTIONS FILE
#  Authors: Jack Williams and Nichola Naylor 

## For this exercise, see the script section headings 
## to see which part of the script correspond
## to sections 1. - 4. in the Intsructions pdf.

### 1. Setting up the model parameters and estimating NMB ####

lambda <- 30000 # This is our willingness to pay threshold
prevalence <- 0.3 # This is the prevalence of the condition

# Here are the outcomes, and the expected costs and QALYs for sick and healthy individuals, treated and not treated. 
outcome.names <- c("Sick person treated", "Sick person not treated", 
                   "Healthy person treated", "Healthy person not treated")
expected.cost <- c(6000, 5000, 3000, 1000)
expected.qaly <- c(0.8, 0.5, 0.95, 1)

# These parameters are combined into a data frame, with the NMB column left blank, to be calculated separately.
parameter.values <- data.frame(outcome.names, expected.cost, expected.qaly,
                              nmb = NA)

# Estimate the NMB associated with each outcome status: 
parameter.values$nmb <- (expected.qaly * lambda) - expected.cost

# here you can see the expected costs, QALYs and NMB for each possible outcome
parameter.values

# first estimate the expected values of treating all versus treating none
# in the complete absence of testing:

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

## We can create a function to calculate NMB for sick and healthy patients (across prevalence values)  
est.nmb <- function(prev, parameters = parameter.values, lam = lambda){
  ## FUNCTION: a function that calculates net monetary benefit for sick and health patients
  ## INPUTS: prev - a numeric values of prevalence, 
  ##         parameters - a data.frame containing "expected.cost" and "expected.qaly" columns
  ##         and row order corresponds to sick person treated, sick person not treated, healthy person treated, healthy person not treated
  ##         lam = a numeric value representing a willingness-to-pay threshold value
  ## OUTPUTS: a data.frame outputting the prevalence used and the calculated
  ##          net monetary benefit for treated and not treated
  
  # Treatment - look up correct costs and outcomes for treated
  
  cost.sick <-  parameters$expected.cost[1]
  cost.healthy <-  parameters$expected.cost[3]

  qaly.sick <-  parameters$expected.qaly[1]
  qaly.healthy <-  parameters$expected.qaly[3]

  cost.treat <- prev * cost.sick + (1-prev) * cost.healthy
  qaly.treat <- prev * qaly.sick + (1-prev) * qaly.healthy

  nmb.treat <- (qaly.treat * lam) - cost.treat

  # No treatment- look up correct costs and outcomes for not treated

  cost.sick <-  parameters$expected.cost[2]
  cost.healthy <-  parameters$expected.cost[4]

  qaly.sick <-  parameters$expected.qaly[2]
  qaly.healthy <-  parameters$expected.qaly[4]

  cost.notreat <- prev * cost.sick + (1-prev) * cost.healthy
  qaly.notreat <- prev * qaly.sick + (1-prev) * qaly.healthy

  nmb.notreat <- (qaly.notreat * lam) - cost.notreat
  
  ## Alternative method, as NMB already estimated
  # nmb.treat <- prev * parameters$nmb[1] + (1-prev)*parameters$nmb[3]
  # nmb.notreat <- prev * parameters$nmb[2] + (1-prev)*parameters$nmb[4]
  
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


# Now we can create a line plot in base R to observe the results:
# fill in the variable names left blank in the blow
# you want prevalence on the x-axis and nmb on the y-axis
# Note - we will be updating this plot in the next section
plot(no.test.nmb$prevalence, no.test.nmb$nmb.treat, type="l", col = "blue", ylim = c(0, 30000))
  lines(no.test.nmb$prevalence, no.test.nmb$nmb.notreat, col = "red")
  legend(0.7, 30000, legend=c("Treat all", "Treat none"), col=c("blue", "red"), 
         lty = 1, cex = 0.8)
  
  # check this against solutions here as we'll be using the parameters 
  # defined in this section throughout the rest of this exercise 

#### 2. Expected value of perfect diagnostic information #####

# In this example, we will assume that all sick patients are treated, and all healthy patients are not


est.nmb.perfect.test <- function(prev, parameters = parameter.values, lam = lambda){
  ## FUNCTION: a function that calculates net monetary benefit for  a perfect test
  ## INPUTS: prev - a numeric values of prevalence, 
  ##         parameters - a data.frame containing "expected.cost", "expected.qaly" and "nmb" columns
  ##         and row order corresponds to sick person treated, sick person not treated, healthy person treated, healthy person not treated
  ##         lam = a numeric value representing a willingness-to-pay threshold value
  ## OUTPUTS: a data.frame outputting the prevalence used and the calculated
  ##          net monetary benefit from a perfect test at that prevalence level
  
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
  
  ## Alternative if NMB already estimated
  # nmb.perfect <- prev * parameters$nmb[1] + (1-prev)*parameters$nmb[4]

  return(data.frame(prevalence = prev, 
                    nmb = nmb.perfect))
}

## Next, run the function and check the results 
# First run the function with the 30% prevalence, then use the prevalence.values previously stored

est.nmb.perfect.test(prev = 0.3) ## 30% prevalence

est.nmb.perfect.test(prev = prevalence.vector) ## using the prevalence vector

# Now save the results using the prevalence values vector 
perfect.test.nmb <- est.nmb.perfect.test(prev = prevalence.vector)

## Compare the results of no.test.nmb (calculated using the est.nmb function) 
# and the perfect.test.nmb (calculated from the est.nmb.perfect.test function) 
no.test.nmb
perfect.test.nmb

## We can also plot the results of the these tables
# (take a look at previous plot functions for more help getting started)
plot(no.test.nmb$prevalence, no.test.nmb$nmb.treat, type="l", col = "red", ylim = c(0, 30000))
lines(no.test.nmb$prevalence, no.test.nmb$nmb.notreat, col = "dark green")
lines(perfect.test.nmb$prevalence, perfect.test.nmb$nmb, col = "blue")
legend(0.7, 30000, legend=c("Treat all", "Treat none", "Perfect test"), col=c("red", "dark green", "blue"), 
       lty = 1, cex = 0.8)


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

## We can now compare this to the values expected from a perfect test
evpdi <- perfect.test.nmb$nmb - no.test.nmb$nmb.max

# and create a data.frame to store the results
evpdi.table <- data.frame(prevalence = prevalence.vector, evpdi = evpdi)
evpdi.table

# We can create a simple plot of the expected value of perfect diagnostic information, by prevalence:
plot(evpdi.table, type="l", col = "blue", ylim = c(0, 3000))

## take a moment to think about what this plot is showing

### 3. Test accuracy for imperfect tests #### 

test.char <- c(positive.mean = 1, positive.sd = 1, 
               negative.mean = -1, negative.sd = 1)

# You can see the test characteristics here
test.char

# Set a range of diagnostic threshold values
diagnostic.threshold <- seq(from = -4, to = 4, by = 0.25)

# Diagnostic positive and negative values, at different diagnostic thresholds
# These are estimated using the normal distribution 
dis.pos <- prevalence * dnorm(diagnostic.threshold, mean = test.char[1], sd = test.char[2])
dis.neg <- (1 - prevalence) * dnorm(diagnostic.threshold, mean = test.char[3], sd = test.char[4])

biomarker.dist <- data.frame(diagnostic.threshold, dis.pos, dis.neg)

# We can view the table, and round to 4 decimal places to help view the numbers
round(biomarker.dist, 4)

# And here we can plot the numbers to re-create the video 
# (note we can use the data frame columns (biomarker.dist), or the vectors that formed the data frame)
plot(diagnostic.threshold, dis.pos, type="l", col = "red", ylim = c(0, 0.3))
lines(diagnostic.threshold, dis.neg, col = "blue")


## True Positive Rate and False Positive Rate 
TPR <- 1 - pnorm(diagnostic.threshold, test.char[1], test.char[2])
FPR <- 1 - pnorm(diagnostic.threshold, test.char[3], test.char[4])

## Note that we use the 'lower.tail=FALSE' option instead of 1-pnorm. The following gives the same results:
1 - pnorm(diagnostic.threshold, test.char[1], test.char[2])
pnorm(diagnostic.threshold, test.char[1], test.char[2], lower.tail = FALSE)

# We can view these alongside the diagnostic thresholds 
positive.rate <- data.frame(diagnostic.threshold, TPR, FPR)


# Plot to view TPR and FPR (Receiver Operating Characteristic (ROC) curve)
plot(FPR, TPR, type="l", col = "red")
# and we can add a diagonal line too
lines(c(0,1),(c(0,1)), col = "grey")


#### 4. Expected Value of Clinical Information #### 

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

# ## Note: An alternative method is to write a function into apply      
# nmb.test.vec <- apply(diag.accuracy, 1, function(x) sum(x * parameter.values$nmb))

# We can view the output of the calculation here
# This is the NMB associated with the imperfect diagnostic test
data.frame(diagnostic.threshold, nmb.test.vec)    

# We can add these results onto the diagnostic accuracy data.frame 
diag.accuracy$nmb <- nmb.test.vec

# Calculate the EVCI

# First, remind yourself of the est.nmb() function from earlier

est.nmb(prev = prevalence.vector)

est.evci <- function(prev, diag = diagnostic.threshold, test = test.char, 
                     nmb.input = parameter.values$nmb, nmb.notest = no.test.nmb){
  ## FUNCTION:  calculates the diagnostic accuracy and associated NMB 
  ##            of an imperfect test (similar to calculations above) and also estimates the NMB 
  ##            in the absense of any test, to estimate the Expected Value of Clinical Information 
  ## INPUTS: prev - a numeric values of prevalence, 
  ##         diagnostic threshold - a vector of diagnostic threshold numeric values
  ##         test - a vector of test characteristics 
  ##         nmb.input - numeric vector of NMB values from different treatment pathways
  ##         nmb.notest - data.frame output similar structure to no.test.nmb
  ## OUTPUTS: a data.frame outputting the prevalence used and the calculated
  ##          net monetary benefit from a perfect test at that prevalence level
  
  
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

# We can create a plot of the EVCI compared to the EVPDI calculated earlier

plot(evpdi.table, type="l", col = "blue", ylim = c(0, 3000))
lines(evci$prev, evci$evci, col = "red")
legend("topright", legend=c("EVPDI", "EVCI"), col=c("blue", "red"), 
       lty = 1, cex = 0.8)

### Additional ggplot Graphs ####

library(reshape2)
library(ggplot2)

# NMB plot

nmb.plot.data <- cbind(no.test.nmb[,1:3], perfect.test.nmb[,2])
colnames(nmb.plot.data) <- c("Prevalence", "Treat all", "Treat none", "Perfect test")
nmb.plot.data.long <- reshape2::melt(nmb.plot.data, id.vars = c("Prevalence"))

nmb.plot  <-  ggplot(nmb.plot.data.long) + 
  geom_line(aes(x=Prevalence, y=value, colour = variable), size=1) + 
  labs(x = "Prevalence", text = element_text(size=10)) + 
  labs(y = "Net Monetary Benefit (£)", text = element_text(size=10)) + theme_classic() +
  theme(legend.title = element_blank(), axis.title=element_text(face="bold"), 
        axis.title.x = element_text(margin = margin(t = 7, r = 0, b = 3, l = 0)), 
        axis.title.y = element_text(margin = margin(t = 0, r = 7, b = 0, l = 3)), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        legend.key.width=unit(1.8,"line"), text = element_text(size=12)) + 
  scale_x_continuous(expand = c(0, 0.1)) + 
  scale_y_continuous(limits = c(0,30000), breaks=seq(0,30000,5000),  expand = c(0, 0))

nmb.plot

# EVDI plot

evci.plot.data <- cbind(evpdi.table, evci$evci)
colnames(evci.plot.data) <- c("Prevalence", "EVPDI", "EVCI") 
evci.plot.data.long <- reshape2::melt(evci.plot.data, id.vars = c("Prevalence"))

evdi.plot  <-  ggplot(evci.plot.data.long) + 
  geom_line(aes(x=Prevalence, y=value, colour=variable), size=1) + 
  labs(x = "Prevalence", text = element_text(size=10)) + 
  labs(y = "Expected Value of Diagnostic Information (£)", text = element_text(size=10)) + theme_classic() +
  theme(legend.title = element_blank(), axis.title=element_text(face="bold"), 
        axis.title.x = element_text(margin = margin(t = 7, r = 0, b = 3, l = 0)), 
        axis.title.y = element_text(margin = margin(t = 0, r = 7, b = 0, l = 3)), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        legend.key.width=unit(1.8,"line"), text = element_text(size=12)) + 
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(limits = c(-1,3000), expand = c(0, 0))

evdi.plot

# Diagnostic threshold

head(biomarker.dist)
colnames(biomarker.dist) <- c("Diagnostic", "Disease Positive", "Disease Negative")
biomarker.dist.long <- reshape2::melt(biomarker.dist, id.vars = c("Diagnostic"))

diag.plot  <-  ggplot(biomarker.dist.long) + 
  geom_line(aes(x=Diagnostic, y=value, colour=variable), size=1) + 
  labs(x = "Diagnostic Threshold", text = element_text(size=10)) + 
  labs(y = "Density", text = element_text(size=10)) + theme_classic() +
  theme(legend.title = element_blank(), axis.title=element_text(face="bold"), 
        axis.title.x = element_text(margin = margin(t = 7, r = 0, b = 3, l = 0)), 
        axis.title.y = element_text(margin = margin(t = 0, r = 7, b = 0, l = 3)), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        legend.key.width=unit(1.8,"line"), text = element_text(size=12)) + 
  scale_x_continuous(expand = c(0, 0.1)) +
  geom_vline(xintercept=0, linetype="dotted")
  
diag.plot

# ROC plot 

roc.data <- data.frame(TPR, FPR)

roc.plot  <-  ggplot(roc.data) + 
  geom_line(aes(x=FPR, y=TPR), size=1) + 
  labs(x = "False Positive Rate (1-Specificity)", text = element_text(size=10)) + 
  labs(y = "True Positive Rate (Sensitivity)", text = element_text(size=10)) + theme_classic() +
  theme(legend.title = element_blank(), axis.title=element_text(face="bold"), 
        axis.title.x = element_text(margin = margin(t = 7, r = 0, b = 3, l = 0)), 
        axis.title.y = element_text(margin = margin(t = 0, r = 7, b = 0, l = 3)), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        legend.key.width=unit(1.8,"line"), text = element_text(size=12)) + 
  scale_x_continuous(limits = c(0,1), breaks=seq(0,1,0.2), expand = c(0, 0.01)) + 
  scale_y_continuous(limits = c(0,1), breaks=seq(0,1,0.2), expand = c(0, 0.01))

roc.plot


