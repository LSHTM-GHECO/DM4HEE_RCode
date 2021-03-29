#  DM4HEE 
#  Exercise 6.6b - Expected value of partial perfect information (EVPPI)
#  Author:  Jack Williams & Nichola Naylor
#  Date created: 25 March 2021
#  Date last edit: 25 March 2021


# The source function runs the EVPI model - so that data / model are avaialable here

source('R_solutions/Exercise 6.6a.R')



## Enter inner and outer loop numbers - note these must be higher than sim.runs 
inner.loops <- 100
outer.loops <- 100


# Note that these need to be less than or equal to sim.runs - since that determins the parameter samples
sim.runs >= inner.loops
sim.runs >= outer.loops



# Generate matrices to store EVPPI results 

inner.results <- matrix(0, inner.loops, 4)
colnames(inner.results) <- c("QALY SP0", "Cost SP0", "QALY NP1", "Cost NP1")

evppi.results.SP0 <- matrix(0, ncol = length(lambda.values), nrow = outer.loops)
colnames(evppi.results.SP0) <- as.character(lambda.values)
evppi.results.NP1 <- evppi.results.SP0 


# Estimate NMB across all the lambda values 
nmb.function <- function(lambda, results){
  
  nmb.table <- matrix(lambda, ncol = length(lambda), nrow = dim(results)[1],  byrow = TRUE) 
  
  SP0 <- ((results[,1] * nmb.table) - results[,2])  
  NP1 <- ((results[,3] * nmb.table) - results[,4])
  
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

## Note that the parameter values have already been sampled for the PSA 
## e.g. the survival parameters to estimate revision rates

head(revision.LP)
tail(revision.LP)


## Now the EVPPI loops will be run - each selected different values for inner and outer loops

## EVPPI loops - NP1 parameter (excl survial parameters)

pb = txtProgressBar(min = 0, max = outer.loops, initial = 0, style = 3)

for(a in 1:outer.loops){
  
  ## 1. Select the 'partial' parameter from the outer loop 
  
  revision.vec <- revision.LP[a,]
  
  
  for(b in 1:inner.loops){
    
    # 2. Select traditional parameters, minus the outer loop parameter
    
    # revision.vec <- revision.LP[a,]
    utilities.vec <- as.numeric(state.utilities[b,])
    state.costs.vec <- as.numeric(state.costs[b,])
    transition.vec <- as.numeric(transitions[b,])
    revision.vec[1] <- revision.LP[b,1]
    revision.vec[3] <- revision.LP[b,3]
    revision.vec[2] <- revision.LP[b,1] + revision.LP[a,4] # NP1 parameter only changes on outer loop (so select 'a' row)
    
    inner.results[b,] <- model.THR() 
    
  }
  
  #after each inner loop PSA, calculate the mean NMB for each tx and store the results
  nmb <- nmb.function(lambda.values, inner.results)
  
  evppi.results.SP0[a,] <- nmb[[1]]
  evppi.results.NP1[a,] <- nmb[[2]]
  
  setTxtProgressBar(pb,a)
}

NP1.evppi <- gen.evppi.results()





## EVPPI loops - Survival parameters

pb = txtProgressBar(min = 0, max = outer.loops, initial = 0, style = 3)

for(a in 1:outer.loops){
  
  ## 1. Select the 'partial' parameter from the outer loop 
  
  revision.vec <- revision.LP[a,]
  
  
  for(b in 1:inner.loops){
    
    # 2. Select traditional parameters, minus the outer loop parameter
    
    # revision.vec <- revision.LP[a,]
    utilities.vec <- as.numeric(state.utilities[b,])
    state.costs.vec <- as.numeric(state.costs[b,])
    transition.vec <- as.numeric(transitions[b,])
    #revision.vec[1] <- revision.LP[a,1] - this has been done in the outer loop
    #revision.vec[3] <- revision.LP[a,3]
    revision.vec[2] <- revision.LP[a,1] + revision.LP[b,4] # NP1 parameter is in the inner loop (not in EVPPI)
    
    inner.results[b,] <- model.THR() 
    
  }
  
  #after each inner loop PSA, calculate the mean NMB for each tx and store the results
  nmb <- nmb.function(lambda.values, inner.results)
  
  evppi.results.SP0[a,] <- nmb[[1]]
  evppi.results.NP1[a,] <- nmb[[2]]
  
  setTxtProgressBar(pb,a)
}

surv.evppi <- gen.evppi.results()





## EVPPI loops - Utilities

pb = txtProgressBar(min = 0, max = outer.loops, initial = 0, style = 3)

for(a in 1:outer.loops){
  
  ## 1. Select the 'partial' parameter from the outer loop 
  
  utilities.vec <- as.numeric(state.utilities[a,])
  
  for(b in 1:inner.loops){
    
    # 2. Select traditional parameters, minus the outer loop parameter
    
    revision.vec <- revision.LP[b,]
    #utilities.vec <- as.numeric(state.utilities[b,])
    state.costs.vec <- as.numeric(state.costs[b,])
    transition.vec <- as.numeric(transitions[b,])
    
    
    inner.results[b,] <- model.THR() 
    
  }
  
  #after each inner loop PSA, calculate the mean NMB for each tx and store the results
  nmb <- nmb.function(lambda.values, inner.results)
  
  evppi.results.SP0[a,] <- nmb[[1]]
  evppi.results.NP1[a,] <- nmb[[2]]
  
  setTxtProgressBar(pb,a)
}

utilities.evppi <- gen.evppi.results()




## EVPPI loops - Cost revision

pb = txtProgressBar(min = 0, max = outer.loops, initial = 0, style = 3)

for(a in 1:outer.loops){
  
  ## 1. Select the 'partial' parameter from the outer loop 
  
  state.costs.vec <- as.numeric(state.costs[a,])
  
  for(b in 1:inner.loops){
    
    # 2. Select traditional parameters, minus the outer loop parameter
    
    revision.vec <- revision.LP[b,]
    utilities.vec <- as.numeric(state.utilities[b,])
    #state.costs.vec <- as.numeric(state.costs[b,])
    transition.vec <- as.numeric(transitions[b,])
    
    
    inner.results[b,] <- model.THR() 
    
  }
  
  #after each inner loop PSA, calculate the mean NMB for each tx and store the results
  nmb <- nmb.function(lambda.values, inner.results)
  
  evppi.results.SP0[a,] <- nmb[[1]]
  evppi.results.NP1[a,] <- nmb[[2]]
  
  setTxtProgressBar(pb,a)
}

cRevision.evppi <- gen.evppi.results()



## EVPPI loops - OMR transition parameters

pb = txtProgressBar(min = 0, max = outer.loops, initial = 0, style = 3)

for(a in 1:outer.loops){
  
  ## 1. Select the 'partial' parameter from the outer loop 
  
  transition.vec <- as.numeric(transitions[a,])
  
  for(b in 1:inner.loops){
    
    # 2. Select traditional parameters, minus the outer loop parameter
    
    revision.vec <- revision.LP[b,]
    utilities.vec <- as.numeric(state.utilities[b,])
    state.costs.vec <- as.numeric(state.costs[b,])
    #transition.vec <- as.numeric(transitions[b,])
    transition.vec[3] <- as.numeric(transitions[b,3]) # overwrite the RRR value so included in PSA
    
    inner.results[b,] <- model.THR() 
    
  }
  
  #after each inner loop PSA, calculate the mean NMB for each tx and store the results
  nmb <- nmb.function(lambda.values, inner.results)
  
  evppi.results.SP0[a,] <- nmb[[1]]
  evppi.results.NP1[a,] <- nmb[[2]]
  
  setTxtProgressBar(pb,a)
}

omr.evppi <- gen.evppi.results()





## EVPPI loops - Re-revision risk parameter

pb = txtProgressBar(min = 0, max = outer.loops, initial = 0, style = 3)

for(a in 1:outer.loops){
  
  ## 1. Select the 'partial' parameter from the outer loop 
  
  transition.vec <- as.numeric(transitions[a,])
  
  for(b in 1:inner.loops){
    
    # 2. Select traditional parameters, minus the outer loop parameter
    
    revision.vec <- revision.LP[b,]
    utilities.vec <- as.numeric(state.utilities[b,])
    state.costs.vec <- as.numeric(state.costs[b,])
    #transition.vec <- as.numeric(transitions[b,])
    transition.vec[1:2] <- as.numeric(transitions[b,1:2]) # overwrite the OMR values within inner loop
    
    inner.results[b,] <- model.THR() 
    
  }
  
  #after each inner loop PSA, calculate the mean NMB for each tx and store the results
  nmb <- nmb.function(lambda.values, inner.results)
  
  evppi.results.SP0[a,] <- nmb[[1]]
  evppi.results.NP1[a,] <- nmb[[2]]
  
  setTxtProgressBar(pb,a)
}

rrr.evppi <- gen.evppi.results()




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



