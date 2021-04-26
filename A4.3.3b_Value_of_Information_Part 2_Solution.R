#  Decision Modelling for Health Economic Evaluation
#  Advanced Course Exercise 4b (Part 2): SOLUTION FILE
#  Authors: Andrew Briggs, Jack Williams & Nichola Naylor


## test new code
# just source function that also returns the parameter's we're interested in
source("A4_VoI_Model_JW.R")
source("ggplot_CEA_functions.R")

# ### First, let's check that the new model can run a PSA and gives the results we expect...
# 
# psa.sims <- 1000
# sim.results <- matrix(0, nrow = psa.sims, ncol = 4)
# 
# for (i in 1:psa.sims){
# sim.results[i,] <- model.THR.voi(RR.vec[i], lambda.vec[i], gamma.vec[i], omr[i,], 
#                                  c.revision.vec[i], tp.rrr.vec[i], state.utilities.df[i,]) 
# }
# 
# # Check PSA results
# apply(sim.results, 2, mean)

# The results are the same as in previous exercises (e.g. Exercise 3b) - so now we can focus on the EVPPI 

#### EVPPI #### 

## Enter inner and outer loop numbers - note these must be higher than sim.runs 
inner.loops <- 100
outer.loops <- 100

# Note that these need to be less than or equal to sim.runs - since that determins the parameter samples
sim.runs >= inner.loops
sim.runs >= outer.loops


# Generate matrices to store EVPPI results 
lambda.values <- seq(0, 50000, 1000)

inner.results <- matrix(0, inner.loops, 4)
colnames(inner.results) <- c("QALY SP0", "Cost SP0", "QALY NP1", "Cost NP1")

evppi.results.SP0 <- matrix(0, ncol = length(lambda.values), nrow = outer.loops)
colnames(evppi.results.SP0) <- as.character(lambda.values)
evppi.results.NP1 <- evppi.results.SP0 



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
    
    # Select traditional parameters, minus the outer loop parameter
      # So the inner loop parameters are subset using 'b' and the outer loop by 'a'

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



