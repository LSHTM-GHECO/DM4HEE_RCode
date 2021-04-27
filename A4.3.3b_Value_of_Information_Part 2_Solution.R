#  Decision Modelling for Health Economic Evaluation
#  Advanced Course Exercise 4b (Part 2): SOLUTION FILE
#  Authors: Andrew Briggs, Jack Williams & Nichola Naylor


## For this exercise, we will seperate out the model code and the script to run the EVPPI, 
  # so as to avoid  having a script that is too long

# Here, we can source the adapted model (i.e. run the whole script in the background) 
  # this will load all of the parameter samples of interest, and will load the 
  # model function within the script. 
  # Make sure you have a look at the script, following the exercise instructions as a guide.
source("A4.3.3b_VoI_Model_Script.R")

# Source functions for plots
source("ggplot_CEA_functions.R")



#### EVPPI example walkthrough #### 



evppi.results.SP0.test <- matrix(0, ncol = 1, nrow = 10)
colnames(evppi.results.SP0) <- as.character("£2,200")
evppi.results.NP1.test <- evppi.results.SP0.test 

inner.results <- matrix(0, inner.loops, 4)
colnames(inner.results) <- c("Cost SP0", "QALY SP0", "Cost NP1", "QALY NP1")

WTP = 2200
  
## The parameter values (10,000 of them each) sampled in the model script 
  # this is the model script sourced above

head(RR.vec)
head(lambda.vec)
head(gamma.vec)
head(omr)
head(c.revision.vec)
head(tp.rrr.vec)
head(state.utilities.df)

# Note that the model function requires that data is selected from the above
 # parameters, and passed as arguments to the function 

model.THR.voi(RR.NP1 = RR.vec[1], lambda = lambda.vec[1], gamma = gamma.vec[1], 
              omr = omr[1,], c.revision = c.revision.vec[1], tp.rrr = tp.rrr.vec[1], 
              state.util = state.utilities.df[1,])


model.THR.voi(RR.NP1 = RR.vec[2], lambda = lambda.vec[2], gamma = gamma.vec[2], 
              omr = omr[2,], c.revision = c.revision.vec[2], tp.rrr = tp.rrr.vec[2], 
              state.util = state.utilities.df[2,])

# Example of EVPPI analysis 

a = 1
  
  # check the value of RR.vec selected (this will go into the model function in the inner loop)
  RR.vec[a]
  
  for(b in 1:100){
    
    # The 'partial' parameter will be changed only in the outer loop - so we can select that using 'a'
    # The parameters included in the inner loop are selected using 'b', so will be sampled 100 times 
    # in the inner loop, whilst the relative risk remains constant.
    
    inner.results[b,] <-  model.THR.voi(RR.NP1 = RR.vec[a], lambda = lambda.vec[b], gamma = gamma.vec[b], omr = omr[b,], 
                                        c.revision = c.revision.vec[b], tp.rrr = tp.rrr.vec[b], state.util = state.utilities.df[b,]) 
    
  }
  
  ## Calculate the NMB (at £2,200 WTP threshold defined above)
  
  # Calculate the results of the inner loop - similar to the PSA calculation - estimating NMB
  SP0.nmb <- ((inner.results[,2] * WTP) - inner.results[,1])  
  NP1.nmb <- ((inner.results[,4] * WTP) - inner.results[,3])
  
  # Use the mean NMB for each treatment and save
  evppi.results.SP0.test[a,] <- mean(SP0.nmb)
  evppi.results.NP1.test[a,] <- mean(NP1.nmb)


## Now return to the above to re-run the code, changing a from 2 to 10, and rerunning each time.   
  
  
  

# After re running the code, check the results here
head(evppi.results.SP0.test)
head(evppi.results.NP1.test)

# Now calculate the EVPPI, based on the results recorded 

# Convert the results into a data frame (this makes using the apply function easier)
evppi.df <- data.frame(evppi.results.SP0.test, evppi.results.NP1.test)

# The current information, given uncertainty in the relative risk parameter
current.info <- max(apply(evppi.df, 2, mean))

# The perfect infromation for the relative risk parameter
perfect.info <- mean(apply(evppi.df, 1, max))

# The EVPPI result (per individual)
evppi <- perfect.info - current.info


## Make sure that you understand what calculations are being done, and why. 
# In the next section, the code will run similar calculating, but will be more complex, 
# as the NMB will be evaluated across a range of WTP values, and analyses will be 
# performed for all the parameters (or parameter groups) of interest.




#### EVPPI Model runs #### 

## Enter inner and outer loop numbers - note these must be higher than sim.runs 
inner.loops <- 100
outer.loops <- 100

# Note that these need to be less than or equal to sim.runs - since that determins the parameter samples
sim.runs >= inner.loops
sim.runs >= outer.loops


# Generate matrices to store EVPPI results 
lambda.values <- seq(0, 50000, 100)

inner.results <- matrix(0, inner.loops, 4)
colnames(inner.results) <- c("Cost SP0", "QALY SP0", "Cost NP1", "QALY NP1")

evppi.results.SP0 <- matrix(0, ncol = length(lambda.values), nrow = outer.loops)
colnames(evppi.results.SP0) <- as.character(lambda.values)
evppi.results.NP1 <- evppi.results.SP0 


## Calculate the effective population, to estimate the population EVPPI 
 # note these parameters are the same as used in Part 1 of the exercise

population <- 40000 
years <- 10
evpi.disc <- 0.06
population.seq <- sum(population * (1/(1+evpi.disc) ^ c(0:(years-1))))
effective.population <- sum(population.seq)




# Estimate NMB across all the lambda values 
nmb.function <- function(lambda, results){
  
  nmb.table <- matrix(lambda, ncol = length(lambda), nrow = dim(results)[1],  byrow = TRUE) 
  
  SP0 <- ((results[,2] * nmb.table) - results[,1])  
  NP1 <- ((results[,4] * nmb.table) - results[,3])
  
  nmb.p <- apply(SP0, 2, mean)
  nmb.t <- apply(NP1, 2, mean) 
  
  return(list(nmb.t, nmb.p))
  
}


## Function to estimate EVPPI across lambda values, using the stored EVPPI results

gen.evppi.results <- function(evppi.results1 = evppi.results.SP0, evppi.results2 = evppi.results.NP1, lambda = lambda.values){
  
  ## calculate the mean NMB for placebo and txa, at each lambda 
  current.info1 <- apply(evppi.results1, 2, mean)
  current.info2 <- apply(evppi.results2, 2, mean)
  
  current.info <- pmax(current.info1, current.info2)
  
  evppi.array <- array(0, dim = c(outer.loops, length(lambda), 2))
  evppi.array[,,1] <- evppi.results1
  evppi.array[,,2] <- evppi.results2
  
  perf.info.sims <- apply(evppi.array, c(1,2), max)
  perf.info <- apply(perf.info.sims, 2, mean)
  
  evppi.results <- c(perf.info - current.info)
  
  evppi <- data.frame(lambda, evppi.results)
  
  return(evppi)
  
}





# EVPPI Sampling 
## Now the EVPPI loops will be run - each selected different values for inner and outer loops


#### EVPPI loops - NP1 parameter   ####

pb = txtProgressBar(min = 0, max = outer.loops, initial = 0, style = 3)

for(a in 1:outer.loops){
  
  for(b in 1:inner.loops){
    
    # The 'partial' parameter will be included in the outer loop - so we can select that using 'a' in the outer loop
    # The parameters included in the inner loop remain are selected using 'b'

    inner.results[b,] <-  model.THR.voi(RR.NP1 = RR.vec[a], lambda = lambda.vec[b], gamma = gamma.vec[b], omr = omr[b,], 
                                     c.revision = c.revision.vec[b], tp.rrr = tp.rrr.vec[b], state.util = state.utilities.df[b,]) 
    
  }
  
  #after each inner loop PSA, calculate the mean NMB for each tx and store the results
  nmb <- nmb.function(lambda.values, inner.results)
  
  evppi.results.SP0[a,] <- nmb[[1]]
  evppi.results.NP1[a,] <- nmb[[2]]
  setTxtProgressBar(pb,a)
  
}

NP1.evppi <- gen.evppi.results()



#### EVPPI loops - Survival parameters  ####

pb = txtProgressBar(min = 0, max = outer.loops, initial = 0, style = 3)

for(a in 1:outer.loops){

  for(b in 1:inner.loops){

    inner.results[b,] <-  model.THR.voi(RR.vec[b], lambda.vec[a], gamma.vec[a], omr[b,], 
                                        c.revision.vec[b], tp.rrr.vec[b], state.utilities.df[b,]) 
  }
  
  #after each inner loop PSA, calculate the mean NMB for each tx and store the results
  nmb <- nmb.function(lambda.values, inner.results)
  evppi.results.SP0[a,] <- nmb[[1]]
  evppi.results.NP1[a,] <- nmb[[2]]
  setTxtProgressBar(pb,a)
}

surv.evppi <- gen.evppi.results()



#### EVPPI loops - Utilities  ####

pb = txtProgressBar(min = 0, max = outer.loops, initial = 0, style = 3)

for(a in 1:outer.loops){
  
  for(b in 1:inner.loops){
  inner.results[b,] <-  model.THR.voi(RR.vec[b], lambda.vec[b], gamma.vec[b], omr[b,], 
                                        c.revision.vec[b], tp.rrr.vec[b], state.utilities.df[a,]) 
  }
  
  #after each inner loop PSA, calculate the mean NMB for each tx and store the results
  nmb <- nmb.function(lambda.values, inner.results)
  evppi.results.SP0[a,] <- nmb[[1]]
  evppi.results.NP1[a,] <- nmb[[2]]
  setTxtProgressBar(pb,a)
}

utilities.evppi <- gen.evppi.results()




#### EVPPI loops - Cost revision  ####

pb = txtProgressBar(min = 0, max = outer.loops, initial = 0, style = 3)

for(a in 1:outer.loops){

  for(b in 1:inner.loops){
    inner.results[b,] <-  model.THR.voi(RR.vec[b], lambda.vec[b], gamma.vec[b], omr[b,], 
                                        c.revision.vec[a], tp.rrr.vec[b], state.utilities.df[b,]) 
  }
  
  #after each inner loop PSA, calculate the mean NMB for each tx and store the results
  nmb <- nmb.function(lambda.values, inner.results)
  evppi.results.SP0[a,] <- nmb[[1]]
  evppi.results.NP1[a,] <- nmb[[2]]
  setTxtProgressBar(pb,a)
}

cRevision.evppi <- gen.evppi.results()



#### EVPPI loops - OMR transition parameters  ####

pb = txtProgressBar(min = 0, max = outer.loops, initial = 0, style = 3)

for(a in 1:outer.loops){

    for(b in 1:inner.loops){
    inner.results[b,] <-  model.THR.voi(RR.vec[b], lambda.vec[b], gamma.vec[b], omr[a,], 
                                        c.revision.vec[b], tp.rrr.vec[b], state.utilities.df[b,]) 
  }
  
  #after each inner loop PSA, calculate the mean NMB for each tx and store the results
  nmb <- nmb.function(lambda.values, inner.results)
  evppi.results.SP0[a,] <- nmb[[1]]
  evppi.results.NP1[a,] <- nmb[[2]]
  setTxtProgressBar(pb,a)
}

omr.evppi <- gen.evppi.results()



#### EVPPI loops - Re-revision risk parameter ####

pb = txtProgressBar(min = 0, max = outer.loops, initial = 0, style = 3)

for(a in 1:outer.loops){

  for(b in 1:inner.loops){
    
    inner.results[b,] <-  model.THR.voi(RR.vec[b], lambda.vec[b], gamma.vec[b], omr[b,], 
                                        c.revision.vec[b], tp.rrr.vec[a], state.utilities.df[b,]) 
    
  }
  
  #after each inner loop PSA, calculate the mean NMB for each tx and store the results
  nmb <- nmb.function(lambda.values, inner.results)
  evppi.results.SP0[a,] <- nmb[[1]]
  evppi.results.NP1[a,] <- nmb[[2]]
  setTxtProgressBar(pb,a)
}

rrr.evppi <- gen.evppi.results()



#### Analysis of EVPPI results #### 

# Create a data frame with all EVPPI results

evppi.wide.patient <- data.frame(NP1.evppi,
                                 surv.evppi[,2],
                                 utilities.evppi[,2],
                                 cRevision.evppi[,2],
                                 omr.evppi[,2],
                                 rrr.evppi[,2])

colnames(evppi.wide.patient) <- c("lambda", "NP1 parameter", "Survival parameters", "Utilities", "Revision cost", "Operative mortality ratios", "Re-revision risk")

# The wide format is because there is a column for each parameter set included in the EVPPI 
head(evppi.wide.patient)

# Transform from per patient EVPPI to population EVPPI 
evppi.wide.pop <- evppi.wide.patient * effective.population
evppi.wide.pop[,1] <- evppi.wide.patient[,1]



# For plotting (in ggplot) the wide format data needs to be converted to long format 
evppi.long.pop <- reshape2::melt(evppi.wide.pop, id.vars = c("lambda"))

# Now the parameter is a variable of it's own (i.e. the EVPPI results are stacked on top of each other)
head(evppi.long.pop)


## ggplot - this function has been imported from the ggplot functions sheet (in graphs folder)
plot.evppi(evppi.long.pop)

# If you want to retrieve the EVPPI at a specific point
subset(evppi.long.pop, lambda==2200)


