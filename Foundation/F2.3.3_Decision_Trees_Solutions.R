#  Decision Modelling for Health Economic Evaluation
#  Foundation Course Exercise 2: SOLUTION FILE
#  Authors: Jack Williams and Nichola Naylor

####**** Parameters ****####  

## Probabilities 
p.hiv <- 0.05 # The prevalence of undetected HIV in the antenatal population 
p.trans.control <- 0.26  # Probability of vertical transmission (unknown HIV)
p.trans.int <- 0.07  # Probability of version transmission (known HIV mother, accepts intervention) 
p.int <- 0.95 # Probability of mother accepting intervention 


## Costs
cost.test <- 10 # The cost of the test 
cost.int <- 800  # Cost of the vertical transmission mitigation intervention 


####**** Decision tree ****####

## Testing group ##  

# Accepts intervention (HIV+)  - Vertical transmission
test.path.1 <- p.hiv * p.int * p.trans.int 
test.cost.1 <- cost.test * 1 + cost.int * 1
test.cases.1 <- 1

# Accepts intervention (HIV+) - No vertical transmission
test.path.2 <- p.hiv * p.int * (1 - p.trans.int) 
test.cost.2 <- cost.test * 1 + cost.int * 1
test.cases.2 <- 0

# Do not accept intervention (HIV+) - Vertical transmission
test.path.3 <- p.hiv * (1 - p.int) * p.trans.control 
test.cost.3 <- cost.test * 1 + cost.int * 0
test.cases.3 <- 1

# Do not accept intervention (HIV+) - No vertical transmission
test.path.4 <- p.hiv * (1 - p.int) * (1 - p.trans.control) 
test.cost.4 <- cost.test * 1 + cost.int * 0
test.cases.4 <- 0

# HIV-
test.path.5 <-  1 - p.hiv
test.cost.5 <- cost.test * 1 + cost.int * 0
test.cases.5 <- 0

# Testing arm probabilities and results 

# Now create a vector containing all the pathway probabilities. Then do the same for 
# the costs and transmissions (cases)

test.probs.vec <- c(test.path.1, test.path.2, test.path.3, test.path.4, test.path.5)
test.costs.vec <- c(test.cost.1, test.cost.2, test.cost.3, test.cost.4, test.cost.5)
test.cases.vec <- c(test.cases.1, test.cases.2, test.cases.3, test.cases.4, test.cases.5)

## Next, multiply the appropriate vectors to get each of the pathway costs
testing.costs <- test.probs.vec * test.costs.vec  ## expected value of costs for each pathway
testing.costs

testing.cases <- test.probs.vec * test.cases.vec ## expected value of cases for each pathways
testing.cases

## For the total costs and cases of the whole intervention arm (i.e. all 5 pathways)
# take the sum of the vectors 
testing.costs.total <- sum(testing.costs)
testing.cases.total <- sum(testing.cases)

# Here we can show and label the results 
testing.results <- (c(costs = testing.costs.total, cases = testing.cases.total))  
testing.results

## No testing group 

# Here we will evaluate the pathway probabilities for the no testing group

# HIV positive and transmission
notest.path.1 <- p.hiv * p.trans.control
notest.cost.1 <- 0
notest.cases.1 <- 1

# HIV positive and no transmission
notest.path.2 <- p.hiv * (1 - p.trans.control)
notest.cost.2 <- 0
notest.cases.2 <- 0
  
# HIV-
notest.path.3 <- 1 - p.hiv
notest.cost.3 <- 0 
notest.cases.3 <- 0
  
# No testing arm probabilities and results 

notest.probs.vec <- c(notest.path.1, notest.path.2, notest.path.3)
notest.cost.vec <- c(notest.cost.1, notest.cost.2, notest.cost.3)
notest.cases.vec <- c(notest.cases.1, notest.cases.2, notest.cases.3)

# Now calculate the payoffs for each of the decision tree pathways
notesting.costs <- notest.probs.vec * notest.cost.vec
notesting.cases <- notest.probs.vec * notest.cases.vec

notesting.costs.total <- sum(notesting.costs)
notesting.cases.total <- sum(notesting.cases)

notesting.results <- (c(costs = notesting.costs.total, nocases = notesting.cases.total))  
notesting.results

####**** Analysis: Incremental results ****#### 

# Reduction in probability of vertical transmission with testing and intervention 
cases.avoided <- notesting.cases.total - testing.cases.total

# Incremental cost of testing and intervention
incr.costs <- testing.costs.total - notesting.costs.total

# Cost per HIV-infected birth avoided
incremental.results <- incr.costs / cases.avoided
incremental.results

#### Example of using vectors to evaluate multiple pathways at once ####

## The first four pathways can be performed using vectors in R (to create a vector of four results)
p.hiv * c(p.int, p.int, 1-p.int, 1-p.int) * c(p.trans.int, 1-p.trans.int, p.trans.control, 1-p.trans.control)

# If we save this vector of results, we can compare to our above results
test.prob.vec.new <-  p.hiv * c(p.int, p.int, 1-p.int, 1-p.int) * c(p.trans.int, 1-p.trans.int, p.trans.control, 1-p.trans.control)

test.prob.vec.new[1:4]  ## This is the new results of four pathways, created from one line of code using vectors 
test.probs.vec[1:4]     ## This is the original results from above, created from combining four pathway probability calculations into a new vector 


