# Loading in data and model
#source("Manuscript/THR_Model.R")

## View the PSA results 
simulation.results

# 
# #### SETTING VALUE OF INFORMATION POPULATION PARAMETERS ####
# population <- 40000 
# years <- 10
# evpi.disc <- 0.06
# population.seq <- population * (1/(1+evpi.disc) ^ c(0:(years-1)))
# effective.population <- sum(population.seq)
# 
# #### EXPECTED VALUE OF PARTIAL PERFECT INFORMATION (EVPPI) ANALYSIS #### 
# 
# ## Enter inner and outer loop numbers - note these must be higher than sim.runs 
# inner.loops <- 100
# outer.loops <- 100
# 
# 
# # Create a matrix to store the inner loop results
# inner.results <- matrix(0, inner.loops, 4)
# colnames(inner.results) <- c("Cost SP0", "QALY SP0", "Cost NP1", "QALY NP1")
# 
# evppi.results.SP0 <- matrix(0, nrow = outer.loops, ncol = length(WTP.values))
# colnames(evppi.results.SP0) <- as.character(WTP.values)
# evppi.results.NP1 <- evppi.results.SP0 
# 
# # creating a function to estimate NMB across WTP values:
# 
# nmb.function <- function(WTP, results){
#   #### FUNCTION: Estimate NMB across all the WTP values 
#   ###  INPUTS: a WTP value vector and a results data.frame
#   ###         "Cost SP0" "QALY SP0" "Cost NP1" "QALY NP1"
#   
#   nmb.table <- matrix(WTP, ncol = length(WTP), nrow = dim(results)[1],  byrow = TRUE) 
#   
#   SP0 <- ((results[,"QALY SP0"] * nmb.table) - results[,"Cost SP0"])  
#   NP1 <- ((results[,"QALY NP1"] * nmb.table) - results[,"Cost NP1"])
#   
#   nmb.p <- apply(SP0, 2, mean)
#   nmb.t <- apply(NP1, 2, mean) 
#   
#   ###  OUTPUTS: a list of the NMB under SP0 (nmb.p) and NP1 (nmb.t)
#   return(list(nmb.t, nmb.p))
#   
# }
# 
# 
# ## Function to estimate EVPPI across WTP values, using the stored EVPPI results
# gen.evppi.results <- function(evppi.results1 = evppi.results.SP0, evppi.results2 = evppi.results.NP1, WTP = WTP.values){
#   ### FUNCTION: Function to estimate EVPPI across WTP values, using the stored EVPPI results
#   ### INPUTS: matrix arrays of evppi.results.SP0 and evppi.results.NP1, and
#   ###         a vector of WTP values
#   
#   ## calculate the mean NMB for current and new treatments, at each WTP 
#   current.info1 <- apply(evppi.results1, 2, mean)
#   current.info2 <- apply(evppi.results2, 2, mean)
#   
#   ## calculate the max NMB (overall) from either current or new treatments, at each WTP
#   current.info <- pmax(current.info1, current.info2)
#   
#   # Create an array 
#   evppi.array <- array(0, dim = c(outer.loops, length(WTP), 2))
#   evppi.array[,,1] <- evppi.results1
#   evppi.array[,,2] <- evppi.results2
#   
#   # calculate the max NMB for each treatment, at each WTP, for each simulation 
#   # This is so that the best treatment can be select for each simulation
#   perf.info.sims <- apply(evppi.array, c(1,2), max)
#   perf.info <- apply(perf.info.sims, 2, mean)
#   
#   # Work out the difference between perfect information (for each simulation) vs. current information, at each WTP
#   evppi.results <- c(perf.info - current.info)
#   
#   ### OUTPUT: a data.frame of different WTP and corresponding evppi values     
#   evppi <- data.frame(WTP, evppi.results)
#   return(evppi)
#   
# }
# 
# 
# 
# # EVPPI Sampling 
# 
# ## Now the EVPPI loops will be run - each selected different values for inner and outer loops
# 
# 
# #### EVPPI loops - NP1 parameter  ####

evppi.df <- data.frame(wtp = WTP.values, 
                         RR = NA, OMR = NA, TP = NA, surv = NA, r.cost = NA, util = NA)
colnames(evppi.df) <- c("WTP", "NP1 Relative risk",  "Operative mortality ratios", "Re-revision risk", "Survival parameters", "Revision cost", "Utilities")


pb = txtProgressBar(min = 0, max = outer.loops, initial = 0, style = 3)

for(j in 1:6){
  
  for(a in 1:outer.loops){
    
    for(b in 1:inner.loops){
      
      # The 'partial' parameter will be included in the outer loop - so we can select that using 'a' in the outer loop
      # The parameters included in the inner loop remain are selected using 'b'

      if(j==1) rr.n <- a else rr.n <- b
      if(j==2) omr.n <- a else omr.n <- b
      if(j==3) tp.n <- a else tp.n <- b
      if(j==4) surv.n <- a else surv.n <- b
      if(j==5) c.rev.n <- a else c.rev.n <- b
      if(j==6) util.n <- a else util.n <- b
      
      inner.results[b,] <-  model.THR(RR.vec[rr.n], omr.df[omr.n,],  tp.rrr.vec[tp.n], 
                                                survival.df[surv.n,],c.revision.vec[c.rev.n], 
                                                state.utilities.df[util.n,], mortality.vec = mortality.vec) 
      
    }
    
    #after each inner loop PSA, calculate the mean NMB for each treatment comparator and store the results
    nmb <- nmb.function(WTP.values, inner.results)
    
    evppi.results.SP0[a,] <- nmb[[1]]
    evppi.results.NP1[a,] <- nmb[[2]]
    setTxtProgressBar(pb,a)
    
  }
  
  evppi.df[,j+1] <- gen.evppi.results()[,2]
  
}
#### thoughts about getting started for looping through sample.output[[i]]
# something like setting up function inputs first
# where sample.output[[i]][j] where j is a or b 
# like for i in 1:7, if sample.output[[x]]==i then j=a, if not j=b 
# j==a


