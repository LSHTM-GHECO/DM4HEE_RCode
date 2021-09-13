# ### Loading useful packages
# library(ggplot2)
# library(reshape2) 
# 
# require("rstudioapi")
# setwd(dirname(getActiveDocumentContext()$path)) # Set working directory to source file
# 
# 
# 
# #  Reading the data needed from csv files
# hazards <- read.csv("hazardfunction.csv", header=TRUE) ## importing the hazard inputs from the regression analysis
# cov.55 <- read.csv("cov55.csv",row.names=1,header=TRUE) ## importing the covariance matrix
# life.table <- read.csv("life-table.csv", header=TRUE) ## importing the life table
# 
# # Setting sampling numbers for probabilistic analysis (EVPPI loops set later on)
# sim.runs <- 100 ## the number of simulation to be performed  
# 
# # 
# {
# #########**** PARAMETERS *****######
# 
# age <- 60
# male <- 0
# 
# sample <- function(age = age, male = male, sim.runs = sim.runs){
# 
# # SETTING CONSTANT PARAMETERS OUTSIDE THE FUNCTION 
# 
# ##### DETERMINISTIC PARAMETERS ######
# ## head to the "PROBABLISTIC PARAMETERS" section to see the 
# ## start of the changes needed to perform EVPPI
# 
# dr.c <- 0.06 ## set the discount rate for costs (6%)
# dr.o <- 0.015 ## set the discount rate for outcomes (1.5%)
# cycles <- 60 ## number of cycles running the model
# state.names <- c("P-THR","successP-THR","R-THR","successR-THR","Death") ## a vector of state names
# n.states <- length(state.names) ## number of states in the model
# seed <- c(1,0,0,0,0) #  Seed the starting states of the model (a vector of 1 and 0s for this exercise)
# 
# ## Cost of standard prosthesis and new prosthesis
# c.SP0 <- 394 ## Cost of standard prosthesis
# c.NP1 <- 579 ## Cost of new prosthesis 1
# 
# ### alpha and beta values
# # FOR TRANSITION PROBABILITY ESTIMATION
# a.PTHR2dead <- 2 ## alpha value for operative mortality from primary surgery
# b.PTHR2dead <- 100- a.PTHR2dead ## beta value for operative mortality from primary surgery
# 
# a.rrr <- 4 ## alpha value for revision risk 
# b.rrr <- 100-a.rrr ## beta value for revision risk
# 
# # HAZARD FUNCTION PARAMETERS
# 
# ## Coefficients - on the log hazard scale
# mn.lngamma <- hazards$coefficient[1] ## Ancilliary parameter in Weibull distribution - equivalent to lngamma coefficient
# mn.cons <- hazards$coefficient[2] ##Constant in survival analysis for baseline hazard
# mn.ageC <- hazards$coefficient[3] ## Age coefficient in survival analysis for baseline hazard
# mn.maleC <- hazards$coefficient[4] ## Male coefficient in survival analysis for baseline hazard
# mn.NP1 <- hazards$coefficient[5]
# mn<-c(mn.lngamma, mn.cons,mn.ageC,mn.maleC,mn.NP1) ## vector of mean values from the regression analysis
# 
# cholm <- t(chol(t(cov.55))) ## lower triangle of the Cholesky decomposition
# 
# # FOR COST ESTIMATION
# c.primary <- 0  ## Cost of a primary THR procedure - 
# ## Note that the cost of the primary procedure is excluded (set to 0): since both arms have this procedure it is assumed to net out of the incremental analysis.  However, if the model was to be used to estimate lifetime costs of THR it would be important to include.
# mn.cRevision <- 5294 ## mean cost of revision surgery
# se.cRevision <- 1487 ## standard error of cost of revision surgery
# a.cRevision <- (mn.cRevision/se.cRevision)^2 ## alpha value for cost of revision surgery 
# b.cRevision <- (se.cRevision^2)/mn.cRevision ## beta value for cost of revision surgery
# c.success <- 0 ## Cost of one cycle in a 'success' state (primary or revision)
# 
# # FOR UTILITY ESTIMATION
# ## during the revision period 
# mn.uSuccessP <- 0.85 ## mean utility value for successful primary prosthesis
# se.uSuccessP <- 0.03 ## standard errror utility value for successful primary prosthesis
# ab.uSuccessP <- mn.uSuccessP*(1-mn.uSuccessP)/(se.uSuccessP^2)-1 ## estimating alpha plus beta (ab)
# a.uSuccessP<-mn.uSuccessP*ab.uSuccessP ## estimating alpha (a)
# b.uSuccessP<-a.uSuccessP*(1-mn.uSuccessP)/mn.uSuccessP ## estimating beta (b)
# 
# mn.uSuccessR<-0.75 ## mean utility value for having a successful Revision THR
# se.uSuccessR<-0.04 ## standard error utility value for having a successful Revision THR
# ab.uSuccessR<-mn.uSuccessR*(1-mn.uSuccessR)/(se.uSuccessR^2)-1 ## alpha + beta (ab)
# a.uSuccessR<-mn.uSuccessR*ab.uSuccessR ## alpha (a)
# b.uSuccessR<-a.uSuccessR*(1-mn.uSuccessR)/mn.uSuccessR ## beta(b)
# 
# mn.uRevision<-0.30 ## mean utility score during the revision period
# se.uRevision<-0.03 ## standard error utility score during the revision period
# ab.uRevision<-mn.uRevision*(1-mn.uRevision)/(se.uRevision^2)-1 ## alpha + beta (ab)
# a.uRevision<-mn.uRevision*ab.uRevision ## alpha (a)
# b.uRevision<-a.uRevision*(1-mn.uRevision)/mn.uRevision ## beta(b)
# 
# ## discount matrices
# cycle.v <- 1:cycles ## a vector of cycle numbers 1 - 60
# discount.factor.c <- 1/(1+dr.c)^cycle.v ## the discount factor matrix
# discount.factor.o <- 1/(1+dr.o)^cycle.v  ## discount factor matrix for utility 
# 
# 
# 
# ## LIFE TABLE DATA  - outside of the model when age and sex are specified upfront 
# # This is included within the function as it varies by age and sex (which are inputs into the function)
# colnames(life.table) <- c("Age","Index","Males","Female") ## making sure column names are correct
# current.age <- age + cycle.v ## a vector of cohort age throughout the model
# interval <- findInterval(current.age, life.table$Index)
# death.risk <- data.frame(age = current.age, males = life.table[interval,3], females = life.table[interval,4])
# col.key <- 3-male 
# mortality.vec <- death.risk[,col.key]
# 
# #########**** PROBABILISTIC PARAMETERS *****######
# 
# # we now want to create vectors & data.frames of the probablistic values:
# ###  Transition probabilities
# tp.PTHR2dead <- rbeta(sim.runs, a.PTHR2dead, b.PTHR2dead) ## OMR following primary THR
# tp.RTHR2dead <- rbeta(sim.runs, a.PTHR2dead, b.PTHR2dead)  ## OMR following revision THR
# 
# ## creating a data.frame of sampled transition probabilites
# omr.df <- data.frame(tp.PTHR2dead, tp.RTHR2dead) 
# 
# tp.rrr.vec <-rbeta(sim.runs, a.rrr, b.rrr) ## Re-revision risk 
# 
# ##  Costs
# c.revision.vec <- rgamma(sim.runs, shape=a.cRevision, scale=b.cRevision) ## Gamma distribution draw for cost of revision surgery
# ## since this is the only probablistic cost sampled we can leave as a vector
# 
# ##  Utilities
# uSuccessP <- rbeta(sim.runs, a.uSuccessP, b.uSuccessP) 
# uSuccessR <- rbeta(sim.runs, a.uSuccessR, b.uSuccessR) 
# uRevision <- rbeta(sim.runs, a.uRevision, b.uRevision) 
# ## Make a data frame to pass into the function
# state.utilities.df <- data.frame(uprimary=rep(0, sim.runs), 
#                                  uSuccessP, uRevision, uSuccessR,
#                                  udeath=rep(0, sim.runs))
# 
# ##  Hazard function ####
# ## Cholesky decomposition
# ## and sampling to create a hazard function data.frame to be passed
# ## through the model
# z <- matrix(rnorm(5*sim.runs, 0, 1), nrow = sim.runs, ncol = 5) ## 5 random draws, by sim.runs
# r.table <- matrix(0, nrow = sim.runs, ncol = 5)
# colnames(r.table) <- c("lngamma", "cons", "age", "male", "NP1")
# 
# for(i in 1:sim.runs){
#   
#   Tz <- cholm %*% z[i,] 
#   x <- mn + Tz 
#   r.table[i,] <- x[,1]
#   
# }
# 
# r <- as.data.frame(r.table)
# gamma.vec <- exp(r$lngamma)
# lambda.vec <- exp(r$cons + age * r$age + male*r$male)
# RR.vec <- exp(r$NP1)
# 
# ## creating a data.frame with the parameters
# ## taken from the hazard function analysis
# survival.df <- data.frame(gamma.vec, lambda.vec)
# # note we just use the vector for rr.NP1 (since we want to investigate) this parameter specifically
# 
# }
# 
# ## Lets take a look at what we've created so far:
# # parameter groups: (data.frames)
# head(survival.df)
# head(state.utilities.df)
# head(omr.df)
# 
# # individual parameter values: (vectors)
# head(tp.rrr.vec)
# head(RR.vec)
# head(c.revision.vec)
# 
# 
# ### now we define an adjusted model function:
# model.THR <- function(RR.NP1, ## from RR.vec
#                           omr,  ## from omr.df
#                           tp.rrr, ## from tp.rrr.vector
#                           survival, ## from survival.df
#                           c.revision, ## from c.revision.vector
#                           state.util) { ## from state.utilities.df
#   ### FUNCTION: Run EVPPI on probablistic parameters wihin the THR model
#   ##           on 6 pre-specified parameter/parameter groups
#   ## INPUTS: 3 data.frame rows (1 row from omr.df, survival.df & state.utilities.df) 
#   ##        and 3 vector values (1 value from vectors; c.revision.vector, tp.rrr.vector, RR.vec)
#   ## OUTPUTS: a labelled numeric output providing costs and qalys for SP0 and NP1, given the set of input parameters
#   
#   ## First, we need to unpack values from the data.frames provided to the function 
#   
#   tp.PTHR2dead <- unlist(omr[1,1])
#   tp.RTHR2dead <- unlist(omr[1,2])
#   
#   state.utilities <- unlist(state.util)
#   
#   gamma <- unlist(survival[1,1])
#   lambda <- unlist(survival[1,2])
#   
#   
#   ## Make sure the revision cost passed into function gets added into state cost vector
#   state.costs <- c(c.primary, c.success, c.revision, c.success, 0) ## a vector with the costs for each state
#   
#   ## Now we continue on with our previous model.THR calculations:
#   
#   # Calculate revision risks 
#   revision.risk.sp0 <- 1- exp(lambda * ((cycle.v-1) ^gamma-cycle.v ^gamma))
#   revision.risk.np1 <- 1- exp(lambda * RR.NP1 * ((cycle.v-1) ^gamma-cycle.v ^gamma))
#   
#   
#   # Transition arrays
#   tm.SP0 <- array(data=0,dim=c(n.states, n.states, cycles),
#                   dimnames= list(state.names, state.names, 1:cycles)) ## an empty array of dimenions (number of states, number of states, number of cycles)
#   
#   # Now using vectorisation to complete this (i.e. not using the loop, to help speed this up)
#   
#   tm.SP0["P-THR","Death",] <- tp.PTHR2dead 
#   tm.SP0["P-THR","successP-THR",] <- 1 - tp.PTHR2dead 
#   tm.SP0["successP-THR","R-THR",] <- revision.risk.sp0 
#   tm.SP0["successP-THR","Death",] <- mortality.vec
#   tm.SP0["successP-THR","successP-THR",] <- 1 - revision.risk.sp0 - mortality.vec
#   tm.SP0["R-THR","Death",] <- tp.RTHR2dead + mortality.vec
#   tm.SP0["R-THR","successR-THR",] <- 1 - tp.RTHR2dead - mortality.vec
#   tm.SP0["successR-THR","R-THR",] <- tp.rrr
#   tm.SP0["successR-THR","Death",] <- mortality.vec
#   tm.SP0["successR-THR","successR-THR",] <- 1 - tp.rrr - mortality.vec
#   tm.SP0["Death","Death",] <- 1 
#   
#   #  Create a trace for the standard prosthesis arm
#   trace.SP0 <- matrix(data=0, nrow=cycles, ncol=n.states)
#   colnames(trace.SP0) <- state.names
#   
#   trace.SP0[1,] <- seed%*%tm.SP0[,,1]
#   
#   for (i in 2:cycles) trace.SP0[i,] <- trace.SP0[i-1,]%*%tm.SP0[,,i]
#   
#   #  NP1 ARM
#   tm.NP1 <- array(data=0,dim=c(n.states, n.states, cycles),
#                   dimnames= list(state.names, state.names, 1:cycles)) ## an empty array of dimenions (number of states, number of states, number of cycles)
#   ## naming all dimensions
#   
#   ### create a loop that creates a time dependent transition matrix for each cycle
#   tm.NP1["P-THR","Death",] <- tp.PTHR2dead ## Primary THR either enter the death state or.. or..
#   tm.NP1["P-THR","successP-THR",] <- 1 - tp.PTHR2dead ## they go into the success THR state 
#   tm.NP1["successP-THR","R-THR",] <- revision.risk.np1 ## revision risk with NP1 treatment arm 
#   tm.NP1["successP-THR","Death",] <- mortality.vec
#   tm.NP1["successP-THR","successP-THR",] <- 1 - revision.risk.np1 - mortality.vec
#   tm.NP1["R-THR","Death",] <- tp.RTHR2dead + mortality.vec
#   tm.NP1["R-THR","successR-THR",] <- 1 - tp.RTHR2dead - mortality.vec
#   tm.NP1["successR-THR","R-THR",] <- tp.rrr
#   tm.NP1["successR-THR","Death",] <- mortality.vec[i]
#   tm.NP1["successR-THR","successR-THR",] <- 1 - tp.rrr - mortality.vec
#   tm.NP1["Death","Death",] <- 1 
#   
#   
#   #  Create a trace for the standard prosthesis arm
#   trace.NP1 <- matrix(data=0,nrow=cycles,ncol=n.states)
#   colnames(trace.NP1) <- state.names
#   
#   trace.NP1[1,] <- seed%*%tm.NP1[,,1]
#   
#   for (i in 2:cycles) trace.NP1[i,] <- trace.NP1[i-1,]%*%tm.NP1[,,i]
#   
#   # COST #
#   
#   cost.SP0 <- trace.SP0%*%state.costs  
#   disc.cost.SP0 <- (discount.factor.c%*%cost.SP0) + c.SP0   
#   
#   cost.NP1 <- trace.NP1%*%state.costs  
#   disc.cost.NP1 <- (discount.factor.c%*%cost.NP1) + c.NP1 
#   
#   # QALYS #
#   QALYs.SP0 <- trace.SP0%*%state.utilities ## utility per cycle
#   disc.QALYs.SP0 <- colSums(discount.factor.o%*%QALYs.SP0) ## total discounted utility
#   
#   QALYs.NP1 <- trace.NP1%*%state.utilities ## utility per cycle
#   disc.QALYs.NP1 <- colSums(discount.factor.o%*%QALYs.NP1) ## total discounted utility
#   
#   
#   output <- c(cost.SP0 = disc.cost.SP0,
#               qalys.SP0 = disc.QALYs.SP0,
#               cost.NP1 = disc.cost.NP1,
#               qalys.NP1 = disc.QALYs.NP1)
#   
#   return(output)
#   
# }
# 
# }


## START OF VOI SCRIPT





#### EVPPI EXAMPLE #### 

# 
# evppi.results.SP0.test <- matrix(0, ncol = 1, nrow = 10) ## creating an empty matrix
# colnames(evppi.results.SP0.test) <- c("£2,200")
# evppi.results.NP1.test <- evppi.results.SP0.test 
# 
# inner.results <- matrix(0, 100, 4) ## empty matrix to store the results of the inner loop
# colnames(inner.results) <- c("Cost SP0", "QALY SP0", "Cost NP1", "QALY NP1")
# 
# WTP <- 2200

# 
# 
# ## The parameter values (10,000 (=sim.runs) of them each) are sampled in the model script 
# # this is the model script sourced above
# # parameter groups: (data.frames)
# head(survival.df)
# head(state.utilities.df)
# head(omr.df)
# 
# # individual parameter values: (vectors)
# head(tp.rrr.vec)
# head(RR.vec)
# head(c.revision.vec)
# 
# # Note that the model function requires that data is selected from the above
# # parameters, and passed as arguments to the function 
# # Testing the model:
# model.THR(RR.vec[1], omr.df[1,],  tp.rrr.vec[1], 
#               survival.df[1,],c.revision.vec[1], 
#               state.utilities.df[1,]) 
# 
# 
# model.THR(RR.vec[2], omr.df[2,],  tp.rrr.vec[2], 
#               survival.df[2,],c.revision.vec[2], 
#               state.utilities.df[2,]) 
# 
# 
# for(b in 1:100){
#   
#   # The 'partial' parameter will be changed only in the outer loop - so we can select that using 'a'
#   # The parameters included in the inner loop are selected using 'b', so will be sampled 100 times 
#   # in the inner loop, whilst the relative risk remains constant.
#   
#   inner.results[b,] <-  model.THR(RR.vec[a], omr.df[b,],  tp.rrr.vec[b], 
#                                       survival.df[b,],c.revision.vec[b], 
#                                       state.utilities.df[b,]) 
#   
# }
# 
# ## Calculate the NMB (at £2,200 WTP threshold defined above)
# 
# # Calculate the results of the inner loop - similar to the PSA calculation - estimating NMB
# SP0.nmb <- ((inner.results[,"QALY SP0"] * WTP) - inner.results[,"Cost SP0"])  
# NP1.nmb <- ((inner.results[,"QALY NP1"] * WTP) - inner.results[,"Cost NP1"])
# 
# # Use the mean NMB for each treatment and save
# evppi.results.SP0.test[a,] <- mean(SP0.nmb)
# evppi.results.NP1.test[a,] <- mean(NP1.nmb)
# 
# ## Now return to the above to re-run the code, changing a from 2 to 10, and rerunning each time.   
# 
# # After re running the code, check the results here
# head(evppi.results.SP0.test)
# head(evppi.results.NP1.test)
# 
# # Now calculate the EVPPI, based on the results recorded 
# 
# # Convert the results into a data frame (this makes using the apply function easier)
# evppi.df <- data.frame(evppi.results.SP0.test, evppi.results.NP1.test)
# 
# # The current information, given uncertainty in the relative risk parameter
# current.info <- max(apply(evppi.df, 2, mean)) ## 2 indicates columns 
# # so we want the maximum value out of the 2 column averages
# # try looking at apply(evppi.df, 2, mean) and then max(apply(evppi.df, 2, mean))
# 
# # The perfect information for the relative risk parameter
# perfect.info <- mean(apply(evppi.df, 1, max)) ## 1 indicates rows
# # so we want the average value of maximum NMB selected for each row
# # try looking at apply(evppi.df, 1, max) and then mean(apply(evppi.df, 1, max))
# 
# # The EVPPI result (per individual)
# evppi <- perfect.info - current.info
# 
# ## Make sure that you understand what calculations are being done, and why. 
# # In the next section, the code will run similar calculations, but will be more complex, 
# # as the NMB will be evaluated across a range of WTP values, and analyses will be 
# # performed for all the parameters (or parameter groups) of interest.
# 
# 
# 


source("THR_Model.R")



## PROBABILISTIC SENSITIVITY ANALYSIS - EXAMPLE

psa.results <- matrix(0, sim.runs, 4)

sample.output$RR.vec
sample.output$mortality.vec

for(i in 1:sim.runs){
  
  psa.results[i,] <-   model.THR(RR.vec[i], omr.df[i,],  tp.rrr.vec[i], 
                                 survival.df[i,],c.revision.vec[i], 
                                 state.utilities.df[i,], mortality.vec = mortality.vec)[1:4] 
}



## EVPI EXAMPLE 


#### ESTIMATING POPULATION EVPI #####

population <- 40000 
years <- 10
evpi.disc <- 0.06

population.seq <- population * (1/(1+evpi.disc) ^ c(0:(years-1)))
effective.population <- sum(population.seq)



#### RUNNING THE SIMULATIONS ########

# sim.runs <- 100 ## the number of simulation runs

## creating an empty data.frame for simulation results to fill:
simulation.results <- data.frame("cost.SP0" = rep(as.numeric(NA), sim.runs), ## use the rep() function to create sim.runs rows of values
                                 "qalys.SP0"= rep(as.numeric(NA),sim.runs),
                                 "cost.NP1" = rep(as.numeric(NA),sim.runs),
                                 "qalys.NP1" = rep(as.numeric(NA), sim.runs))


## running the simulations and filling the simulation.results data.frame:
for(i in 1:sim.runs){
  simulation.results[i,] <- model.THR(RR.vec[i], omr.df[i,],  tp.rrr.vec[i], 
                                      survival.df[i,],c.revision.vec[i], 
                                      state.utilities.df[i,], mortality.vec = mortality.vec)[1:4]  ## running the model 
}




## EVPI ACROSS A RANGE OF WTP VALUES

## create a vector from 0 to 50,000 in increments of 100 
WTP.values <- seq(from = 0, to = 50000, by = 100)

est.EVPI.pop <-function(WTP, effective.population,
                        simulation.results) {
  #### FUNCTION: estimating EVPI for effective populations
  ###  INPUTS: WTP - numeric WTP value
  ###          effective.population - numeric effective population value
  ###          results - data.table object from simulation.runs with 
  ###          qalys.SP0 and cost.SP0 columns & equivalents for NP1
  ###  OUTPUTS: A numeric value for population EVPI
  
  nmb.SP0 <- simulation.results$qalys.SP0 * WTP - simulation.results$cost.SP0
  nmb.NP1 <- simulation.results$qalys.NP1 * WTP - simulation.results$cost.NP1
  nmb.table <- data.frame(nmb.SP0, nmb.NP1)
  
  av.current <- apply(nmb.table, 2, mean)
  max.current <- max(av.current)
  max.nmb <- apply(nmb.table, 1, max) 
  av.perfect <- mean(max.nmb)
  
  EVPI.indiv <- av.perfect - max.current
  pop.EVPI <- effective.population * EVPI.indiv
  
  return(pop.EVPI)
  
}

## create a data frame to capture the WTP values and subsequent population EVPI results 

EVPI.results <- data.frame(WTP=WTP.values, EVPI=NA)

for (i in 1:length(WTP.values)) {
  EVPI.results[i,2] <- est.EVPI.pop(WTP.values[i], 
                                    effective.population,
                                    simulation.results)
}

# View start of the results
head(EVPI.results)



# plot results 


## ggplot to observe results (the functions created are from from the ggplot functions script)

# Cost-effectiveness plane 
#incremental.results <- simulation.results[,c("inc.qalys","inc.cost")]
#plot.ce.plane(incremental.results)

# Cost-effectiveness acceptability curve 
#plot.ceac(CEAC)

# EVPI, per population

evpi.plot <- ggplot(EVPI.results) + geom_line(aes(x=WTP, y=EVPI), size=1) + 
  labs(x = "Willingness to pay threshold", text = element_text(size=10)) + 
  labs(y = "Expected Value of Perfect Information", text = element_text(size=10)) + theme_classic() +
  theme(legend.title = element_blank(), axis.title=element_text(face="bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        legend.key.width=unit(1.8,"line"), text = element_text(size=12),
        axis.title.x = element_text(margin = margin(t = 7, r = 0, b = 3, l = 0)), 
        axis.title.y = element_text(margin = margin(t = 0, r = 7, b = 0, l = 3)), 
        plot.margin=unit(c(0.5,0.5,0,0.5),"cm")) + 
  scale_x_continuous(labels = scales::comma, expand = c(0, 0.1)) + 
  scale_y_continuous(labels = scales::comma, expand = c(0, 0))

evpi.plot



## the peak occurs where the EVPI is at it's highest;
EVPI.results[EVPI.results$EVPI == max(EVPI.results$EVPI),] ## returns the row with the max EVPI value
## in other words, when the WTP value is around £2,100 per QALY gained (in our test run, the number may be slightly different in your simulations),
## there is the largest expected value of perfected information






#### EVPPI Model runs #### 

## Enter inner and outer loop numbers - note these must be higher than sim.runs 
inner.loops <- 100
outer.loops <- 100

# Note that these need to be less than or equal to sim.runs since that determines the parameter samples
sim.runs >= inner.loops
sim.runs >= outer.loops

# Generate matrices to store EVPPI results 
WTP.values <- seq(0, 50000, 100)

inner.results <- matrix(0, inner.loops, 4)
colnames(inner.results) <- c("Cost SP0", "QALY SP0", "Cost NP1", "QALY NP1")

evppi.results.SP0 <- matrix(0, nrow = outer.loops, ncol = length(WTP.values))
colnames(evppi.results.SP0) <- as.character(WTP.values)
evppi.results.NP1 <- evppi.results.SP0 



# creating a function to estimate NMB across WTP values:

nmb.function <- function(WTP, results){
  #### FUNCTION: Estimate NMB across all the WTP values 
  ###  INPUTS: a WTP value vector and a results data.frame
  ###         "Cost SP0" "QALY SP0" "Cost NP1" "QALY NP1"

  nmb.table <- matrix(WTP, ncol = length(WTP), nrow = dim(results)[1],  byrow = TRUE) 
  
  SP0 <- ((results[,"QALY SP0"] * nmb.table) - results[,"Cost SP0"])  
  NP1 <- ((results[,"QALY NP1"] * nmb.table) - results[,"Cost NP1"])
  
  nmb.p <- apply(SP0, 2, mean)
  nmb.t <- apply(NP1, 2, mean) 
    
  ###  OUTPUTS: a list of the NMB under SP0 (nmb.p) and NP1 (nmb.t)
  return(list(nmb.t, nmb.p))
  
}


## Function to estimate EVPPI across WTP values, using the stored EVPPI results
gen.evppi.results <- function(evppi.results1 = evppi.results.SP0, evppi.results2 = evppi.results.NP1, WTP = WTP.values){
  ### FUNCTION: Function to estimate EVPPI across WTP values, using the stored EVPPI results
  ### INPUTS: matrix arrays of evppi.results.SP0 and evppi.results.NP1, and
  ###         a vector of WTP values
  
  ## calculate the mean NMB for current and new treatments, at each WTP 
  current.info1 <- apply(evppi.results1, 2, mean)
  current.info2 <- apply(evppi.results2, 2, mean)
  
  ## calculate the max NMB (overall) from either current or new treatments, at each WTP
  current.info <- pmax(current.info1, current.info2)
  
  # Create an array 
  evppi.array <- array(0, dim = c(outer.loops, length(WTP), 2))
  evppi.array[,,1] <- evppi.results1
  evppi.array[,,2] <- evppi.results2
  
  # calculate the max NMB for each treatment, at each WTP, for each simulation 
  # This is so that the best treatment can be select for each simulation
  perf.info.sims <- apply(evppi.array, c(1,2), max)
  perf.info <- apply(perf.info.sims, 2, mean)
  
  # Work out the difference between perfect information (for each simulation) vs. current information, at each WTP
  evppi.results <- c(perf.info - current.info)
  
  ### OUTPUT: a data.frame of different WTP and corresponding evppi values     
  evppi <- data.frame(WTP, evppi.results)
  return(evppi)
  
}



# EVPPI Sampling 

## Now the EVPPI loops will be run - each selected different values for inner and outer loops


#### EVPPI loops - NP1 parameter  ####

pb = txtProgressBar(min = 0, max = outer.loops, initial = 0, style = 3)

for(a in 1:outer.loops){
  
  for(b in 1:inner.loops){
    
    # The 'partial' parameter will be included in the outer loop - so we can select that using 'a' in the outer loop
    # The parameters included in the inner loop remain are selected using 'b'
    
    inner.results[b,] <-  model.THR(RR.vec[a], omr.df[b,],  tp.rrr.vec[b], 
                                        survival.df[b,],c.revision.vec[b], 
                                        state.utilities.df[b,], mortality.vec = mortality.vec)[1:4]
    
  }
  
  #after each inner loop PSA, calculate the mean NMB for each treatment comparator and store the results
  nmb <- nmb.function(WTP.values, inner.results)
  
  evppi.results.SP0[a,] <- nmb[[1]]
  evppi.results.NP1[a,] <- nmb[[2]]
  setTxtProgressBar(pb,a)
  
}

NP1.evppi <- gen.evppi.results()



#### EVPPI loops - OMR transition parameters  ####

pb = txtProgressBar(min = 0, max = outer.loops, initial = 0, style = 3)

for(a in 1:outer.loops){
  
  for(b in 1:inner.loops){
    inner.results[b,] <-  model.THR(RR.vec[b], omr.df[a,],  tp.rrr.vec[b], 
                                        survival.df[b,],c.revision.vec[b], 
                                        state.utilities.df[b,], mortality.vec = mortality.vec)[1:4]
  }
  
  #after each inner loop PSA, calculate the mean NMB for each tx and store the results
  nmb <- nmb.function(WTP.values, inner.results)
  evppi.results.SP0[a,] <- nmb[[1]]
  evppi.results.NP1[a,] <- nmb[[2]]
  setTxtProgressBar(pb,a)
}

omr.evppi <- gen.evppi.results()

#### EVPPI loops - Re-revision risk parameter ####

pb = txtProgressBar(min = 0, max = outer.loops, initial = 0, style = 3)

for(a in 1:outer.loops){
  
  for(b in 1:inner.loops){
    
    inner.results[b,] <-  model.THR(RR.vec[b], omr.df[b,],
                                        tp.rrr.vec[a], 
                                        survival.df[b,],c.revision.vec[b], 
                                        state.utilities.df[b,], mortality.vec = mortality.vec)[1:4]
    
  }
  
  #after each inner loop PSA, calculate the mean NMB for each tx and store the results
  nmb <- nmb.function(WTP.values, inner.results)
  evppi.results.SP0[a,] <- nmb[[1]]
  evppi.results.NP1[a,] <- nmb[[2]]
  setTxtProgressBar(pb,a)
}

rrr.evppi <- gen.evppi.results()

#### EVPPI loops - Survival parameters  ####

pb = txtProgressBar(min = 0, max = outer.loops, initial = 0, style = 3)

for(a in 1:outer.loops){
  
  for(b in 1:inner.loops){
    
    inner.results[b,] <-  model.THR(RR.vec[b], omr.df[b,],
                                        tp.rrr.vec[b], 
                                        survival.df[a,],c.revision.vec[b], 
                                        state.utilities.df[b,], mortality.vec = mortality.vec)[1:4]
  }
  
  #after each inner loop PSA, calculate the mean NMB for each tx and store the results
  nmb <- nmb.function(WTP.values, inner.results)
  evppi.results.SP0[a,] <- nmb[[1]]
  evppi.results.NP1[a,] <- nmb[[2]]
  setTxtProgressBar(pb,a)
}

surv.evppi <- gen.evppi.results()

#### EVPPI loops - Cost revision  ####

pb = txtProgressBar(min = 0, max = outer.loops, initial = 0, style = 3)

for(a in 1:outer.loops){
  
  for(b in 1:inner.loops){
    inner.results[b,] <-  model.THR(RR.vec[b], omr.df[b,],tp.rrr.vec[b], 
                                        survival.df[b,],
                                        c.revision.vec[a], 
                                        state.utilities.df[b,], mortality.vec = mortality.vec)[1:4]
  }
  
  #after each inner loop PSA, calculate the mean NMB for each tx and store the results
  nmb <- nmb.function(WTP.values, inner.results)
  evppi.results.SP0[a,] <- nmb[[1]]
  evppi.results.NP1[a,] <- nmb[[2]]
  setTxtProgressBar(pb,a)
}

cRevision.evppi <- gen.evppi.results()

#### EVPPI loops - Utilities  ####

pb = txtProgressBar(min = 0, max = outer.loops, initial = 0, style = 3)

for(a in 1:outer.loops){
  
  for(b in 1:inner.loops){
    inner.results[b,] <-  model.THR(RR.vec[b], omr.df[b,],tp.rrr.vec[b], 
                                        survival.df[b,],c.revision.vec[b], 
                                        state.utilities.df[a,], mortality.vec = mortality.vec)[1:4] 
  }
  
  #after each inner loop PSA, calculate the mean NMB for each tx and store the results
  nmb <- nmb.function(WTP.values, inner.results)
  evppi.results.SP0[a,] <- nmb[[1]]
  evppi.results.NP1[a,] <- nmb[[2]]
  setTxtProgressBar(pb,a)
}

utilities.evppi <- gen.evppi.results()

#### Analysis of EVPPI results #### 

# Create a data frame with all EVPPI results

evppi.wide.patient <- data.frame(NP1.evppi,
                                 surv.evppi[,2],
                                 utilities.evppi[,2],
                                 cRevision.evppi[,2],
                                 omr.evppi[,2],
                                 rrr.evppi[,2])

colnames(evppi.wide.patient) <- c("WTP", "NP1 Relative risk", "Survival parameters", "Utilities", "Revision cost", "Operative mortality ratios", "Re-revision risk")

# The wide format is because there is a column for each parameter set included in the EVPPI 
head(evppi.wide.patient)

# Transform from per patient EVPPI to population EVPPI 
evppi.wide.pop <- evppi.wide.patient * effective.population
evppi.wide.pop[,1] <- evppi.wide.patient[,1]

# For plotting (in ggplot) the wide format data needs to be converted to long format 
evppi.long.pop <- reshape2::melt(evppi.wide.pop, id.vars = c("WTP"))

# Now the parameter is a variable of it's own (i.e. the EVPPI results are stacked on top of each other)
head(evppi.long.pop, 10)


## Plot
plot.evppi <- ggplot(evppi.long.pop) + geom_line(aes(x=WTP, y=value, colour = variable), size=0.75) + 
                labs(x = "Willingness to pay threshold", text = element_text(size=10)) + 
                labs(y = "EVPPI", text = element_text(size=10)) + theme_classic() +
                theme(legend.title = element_blank(), axis.title=element_text(face="bold"), 
                      axis.title.x = element_text(margin = margin(t = 7, r = 0, b = 3, l = 0)), 
                      axis.title.y = element_text(margin = margin(t = 0, r = 7, b = 0, l = 3)), 
                      panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                      legend.key.width=unit(1.8,"line"), text = element_text(size=12),
                      plot.margin=unit(c(0.5,0,0,0.5),"cm")) + 
                scale_x_continuous(labels = scales::comma, limits = c(0, 10000), expand = c(0, 0.1)) + 
                scale_y_continuous(labels = scales::comma, expand = c(0, 0))

plot.evppi
# Note you will get a warning here that the plot does not show all the data in the data.frame
# This is because the plot only shows WTP from 0 to 15000, rather than the 50000 maximum in the data.frame


# If you want to retrieve the EVPPI at a specific point
subset(evppi.long.pop, WTP==2200)

# and we can plot this:
sub.evppi <- subset(evppi.long.pop, WTP==2200)

plot.sub.evppi <- ggplot(sub.evppi, aes(x=variable, y=value)) +
                    geom_bar(stat="identity", fill="steelblue")+
                    labs(x = "Parameter Group", text = element_text(size=4)) + 
                    labs(y = "EVPPI", text = element_text(size=4)) + theme_classic() +
                    theme(legend.title = element_blank(), axis.title=element_text(face="bold"), 
                          axis.title.x = element_text(margin = margin(t = 7, r = 0, b = 3, l = 0)), 
                          axis.title.y = element_text(margin = margin(t = 0, r = 7, b = 0, l = 3)),
                          axis.text.x=element_text(angle=45,hjust=1), 
                          panel.grid.major = element_line(), panel.grid.minor = element_line(), 
                          legend.key.width=unit(1.8,"line"), text = element_text(size=12)) + 
                    scale_y_continuous(labels = scales::comma, expand = c(0, 0))

plot.sub.evppi
