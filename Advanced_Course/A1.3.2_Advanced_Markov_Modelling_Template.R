#  Decision Modelling for Health Economic Evaluation
#  Advanced Course Exercise 1: TEMPLATE FILE
#  Authors: Andrew Briggs, Jack Williams & Nichola Naylor

### !!! set working directory as the folder this is stored in
## added this in to allow for the running of instruction pdf knitting
## whilst reading in data from the same subfolder
## students can ignore if not re-knitting the pdfs, just make sure data files
## are stored in the same file as template/solution files

# require("rstudioapi")  
# setwd(dirname(getActiveDocumentContext()$path)) # Set working directory to source file 


#########**** PARAMETERS *****######
#  Start by defining parameters
#  Demographics & Discount rates

age <- 60 ## set age group for analyses
male <- 0 ## set sex identified, 0 = female and 1 = male
## the specific number (0,1) becomes important for reasons you'll see further down the script
dr.c <- 0.06 ## set the discount rate for costs (6%)
dr.o <- 0.015 ## set the discount rate for outcomes (1.5%)

cycles <- 60   ## number of cycles

state.names <- c("P-THR","successP-THR","R-THR","successR-THR","Death")
n.states <- length(state.names)

#  Seed the starting states of the model
seed <- c(1,0,0,0,0) ## all people start in the first state (P-THR), with state
## order defined inline with state.names 

#  Transition probabilities
tp.PTHR2dead <-     ## Operative mortality rate (OMR) following primary THR
tp.RTHR2dead <-     ##Operative mortality rate (OMR) following revision THR
tp.rrr <-        ## Re-revision risk (assumed to be constant)

#  Costs
c.SP0 <- 394 ## Cost of standard prosthesis
c.NP1 <- 579 ## Cost of new prosthesis 1

c.primary <- 0  ## Cost of a primary THR procedure 
## Note that the cost of the primary procedure is excluded (set to 0): since both arms have this procedure it is assumed to net out of the incremental analysis.  However, if the model was to be used to estimate lifetime costs of THR it would be important to include.
c.success <- 0 ## Cost of one cycle in a 'success' state (primary or revision)
## Note for c.success There are assumed to be no ongoing monitoring costs for successful THR.  However, this parameter is included in case users want to change this assumption.

c.revision <-     ## Cost of one cycle in the Revision THR state (national reference costs for revision hip or knee)

state.costs <- c()    ## a vector with the costs for each state

# Life years
state.lys <-     ## a vector of life year effects for each state

#  Utilities
u.success.p <-      ## Utility score for having had a successful Primary THR
u.revision <-      ## Utility score during the revision period
u.success.r <-      ## Utility score for having a successful Revision THR
u.primary <-        ## Utility for primary procedure
   
state.utilities <- c()     ## a vector with the utilities for each state

#### HAZARD FUNCTION & ASSOCIATED PARAMETERS #####

hazards <- read.csv("hazardfunction.csv") ## importing the hazard inputs from the regression analysis

## Coefficients - on the log hazard scale
r.lnlambda <- hazards$coefficient[1] ## Ancilliary parameter in Weibull distribution - equivalent to lngamma coefficient
r.cons <-          ##Constant in survival analysis for baseline hazard
r.ageC <-         ## Age coefficient in survival analysis 
r.maleC <-         ## Male coefficient in survival analysis 
r.NP1 <-          ## NP1 coefficient in survival analysis 

gamma <- exp(r.lnlambda)
lambda <- exp(  )
RR.NP1 <- exp(  )

##### LIFE TABLES #####
#  Read in the life table
life.table <- read.csv("life-table.csv") ## importing the life table csv inputs
colnames(life.table) <- c("Age","Index","Males","Female") ## making sure column names are correct

cycle.v <-        ## a vector of cycle numbers 1 - 60
current.age <-    ## a vector of cohort age throughout the model
current.age

## Creating a table that has every age of the cohort plus death risks associated with that age
# This finds the position of age, within the life table 
interval <- findInterval(current.age, life.table$Index)
# These positions can then be used to subset the appropriate values from life.table
death.risk <- data.frame(age = current.age, 
                         males = life.table[interval,3],
                         females = life.table[interval,4])

####**** STANDARD *****#####

## defining the revision risks based on the parameters calculated above and cycle vector
revision.risk.sp0 <-    ## the time dependent risk of revision for standard treatment
revision.risk.sp0

revision.risk.np1 <-    ## the time dependent risk of revision for NP1
revision.risk.np1


# combining risks into a time-dependent transition probability data.frame
tdtps <-    ## combine in the order death.risk, revision.risk.sp0, revision.risk.np1
tdtps

## creating an indicator which selects the death risk column depending on the sex the model is being run on
col.key <- 3-male ## 3 indicates the 3rd column of tdps (which is female risk of death)
## when male=1 (i.e. male selected as sex) this becomes the 2nd column (which is male risk of death)

#  Now create a transition matrix for the standard prosthesis arm
#  We start with a three dimensional array in order to capture the time dependencies
tm.SP0 <- array(data = , dim = c( , , ),
                dimnames= list(state.names, state.names, 1:cycles))   # an empty array of dimensions (number of states, number of states, number of cycles)
                ## naming all dimensions

### create a loop that creates a time dependent transition matrix for each cycle
for (i in 1:cycles) {
  

  ## tranisitions out of P-THR
  ## remember you can refer to transitions using state names such as... 
  tm.SP0["P-THR","Death",i] <- tp.PTHR2dead ## Primary THR either enter the death state or.. or..
  tm.SP0["P-THR","successP-THR",i] <- 1 - tp.PTHR2dead ## they go into the success THR state 
  
  ## transitions out of success-P-THR
  tm.SP0["successP-THR","R-THR",i] <- revision.risk.sp0[i] # you could also refer to the corresponding tdtps column
  tm.SP0["successP-THR","Death",i] <- death.risk[i,col.key]
  tm.SP0["successP-THR","successP-THR",i] <- 1-revision.risk.sp0[i] - death.risk[i,col.key]
  
  ## Now complete the remaining transitions out of R-THR 
  tm.SP0["R-THR","Death",i] <- 
  tm.SP0["R-THR","successR-THR",i] <- 
  
  ## and transitions out of success-THR
  tm.SP0["successR-THR","R-THR",i] <- 
  tm.SP0["successR-THR","Death",i] <- 
  tm.SP0["successR-THR","successR-THR",i] <- 
  
  tm.SP0["Death","Death",i] <- 1 ## no transitions out of death
}

tm.SP0

#  Create a trace for the standard prosthesis arm
trace.SP0 <- matrix(data=0, nrow=cycles, ncol=n.states)
colnames(trace.SP0) <- state.names

trace.SP0[1,] <-  %*%  ## the first transition from cycle0 (seed) to cycle1 (hint - use %*% for matrix multiplication)

for (i in 2:cycles) {  ## a loop filling in the rest of the trace matrix
  trace.SP0[i,] <- 
}

trace.SP0

###Life Years####

lys.SP0 <- trace.SP0%*%state.lys 
lys.SP0

undisc.lys.SP0 <-  ## now take the total life years from the lys.SP0 vector
undisc.lys.SP0

###QALYS######

QALYs.SP0 <- ### trace.SP0 multiplied by state.utilities 
QALYs.SP0  ## undiscounted QALYs from SP0 for each cycle

undisc.QALYs.SP0 <-   ## total quality-adjusted life years from lys.SP0
undisc.QALYs.SP0

## DISCOUNTING:

discount.factor.o <-    ## a vector of a discount factor multiplier for each cycle
  ## one way to do this is to simply multiply the cycle vector (cycle.v) with the discount formulae
discount.factor.o

disc.QALYs.SP0 <-    ## discounted QALYs from standard treatment
disc.QALYs.SP0

###COSTS###

cost.SP0 <-      ### trace.SP0 multiplied by state costs
cost.SP0

undisc.cost.SP0 <-   ### total costs, don't forget about any one off cycle0 costs that needed to be added here also
undisc.cost.SP0

## DISCOUNTING
discount.factor.c <- 

disc.cost.SP0 <-        ## discounted total costs
disc.cost.SP0

##########**** NP1 *****######

tm.NP1 <-   ## an empty array of dimensions (number of states, number of states, number of cycles)

  
### create a loop that creates a time dependent transition matrix for each cycle
for (i in 1:cycles) {

  ## tranisitions out of P-THR
  tm.NP1["P-THR","Death",i] <- 
  tm.NP1["P-THR","successP-THR",i] <- 
  ## transitions out of success-P-THR
  tm.NP1["successP-THR","R-THR",i] <-   ## revision risk with NP1 treatment arm 
  tm.NP1["successP-THR","Death",i] <- 
  tm.NP1["successP-THR","successP-THR",i] <- 
  ## transitions out of R-THR 
  tm.NP1["R-THR","Death",i] <- 
  tm.NP1["R-THR","successR-THR",i] <-  
  ## transitions out of success-THR
  tm.NP1["successR-THR","R-THR",i] <- 
  tm.NP1["successR-THR","Death",i] <- 
  tm.NP1["successR-THR","successR-THR",i] <- 
  
  tm.NP1["Death","Death",i] <-    ## no transitions out of death
}

tm.NP1

#  Create a trace for the standard prosthesis arm
trace.NP1 <- matrix(data=0,nrow=cycles,ncol=n.states)
colnames(trace.NP1) <- state.names

trace.NP1[1,] <-  ## the first transition to cycle1 from cycle0 for NP1
  
for (i in 2:cycles) {
  
}
trace.NP1

rowSums(trace.NP1)


###Life Years####
lys.NP1 <-   ## life years from NP1 pathway
lys.NP1

undisc.lys.NP1 <-  ## total life years from NP1 pathway
undisc.lys.NP1

###QALYS######
QALYs.NP1 <-   ## undiscounted QALYs from NP1 for each cycle
QALYs.NP1

undisc.QALYs.NP1 <-  
undisc.QALYs.NP1

## DISCOUNTING:
## can use the same discount.factor.o as created previously 
disc.QALYs.NP1 <-  ## total discounted QALYs for NP1 treatment pathway


###COSTS###
cost.NP1 <-  ## undiscounted costs from NP1 for each cycle
cost.NP1

undisc.cost.NP1 <-  ## total undiscounted costs (including one-off costs)
undisc.cost.NP1

## DISCOUNTING
disc.cost.NP1 <-   ## discounted costs from NP1 pathway (including one-off costs)
disc.cost.NP1


####****ANALYSIS****####

output <- c(inc.cost = , ## incremental cost
            inc.qalys =  ,  ## incremental effect 
            icer = NA)  ## incremental cost-effectiveness ratio
output["icer"] <- 

output



