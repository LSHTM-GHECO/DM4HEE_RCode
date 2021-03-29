#  DM4HEE 
#  Exercise 5.7 - Analysing THR model simulation results
#  Author: Andrew Briggs
#  Date created: 22 February 2021
#  Date last edit: 15 March 2021

# library(dplyr)

#  Make sure you have already created the function: model.THR(age,male)

# There needs to be a way to call and run the other model - one option may be to have the function in a seperate sheet (to call)
# source("R_solutions/Exercise 4.8 with sim.R")

#Note to self - is there a way to use 'replicate' and convert to dataframe to avoid using'rdply' from the 'plyr' library?
#Problem is that replicate gives n*2d vectors and creating a dataframe generates 2rows and n columns
#Solution: use t() to transpose before making into a data.frame
#Using system.time() to check the two functions reveals slight gain for avoiding 'rdply' but no opportunity to use option '.progress="text"'


## Need to source the appropriate R sheet, or a copy of the model - perhaps do a seperate model script 
source("graphs/ggplot functions.R")






#  Run Model

sim.runs<-1000

simulation.results <- data.frame(matrix(data = 0, sim.runs, 2))
colnames(simulation.results)<-c("inc.QALYs","inc.costs")

for(i in 1:sim.runs)   simulation.results[i,] <- model.THR(60,0)


# Plots 

plot(simulation.results$inc.QALYs,simulation.results$inc.cost)

# This is a ggplot function from the ggplot functions script, that is run at the top of this R script
ce.plane(simulation.results)




# Estimate the probability of cost-effectiveness for a given lambda

pCE<-function(lambda, results = simulation.results) {

  nmb <- results[,1]*lambda - results[,2] 
  CE <- nmb>0
  probCE<- mean(CE)
  
  return(probCE)
  
}


# Generate CEAC table

lambda.values <- seq(from = 0, to = 50000, by = 10)
CEAC <- data.frame(matrix(data = NA, nrow = length(lambda.values), ncol = 2))
colnames(CEAC)<-c("lambda","pCE")

for (i in 1:length(lambda.values)) {
  CEAC[i,1] <- lambda.values[i]
  CEAC[i,2]<- pCE(lambda.values[i], simulation.results)
}

# Display the top and bottom of the CEAC table
head(CEAC, 10)
tail(CEAC, 10)


# Plots 

plot(CEAC$lambda,CEAC$pCE, type="l")

## ggplot function 
plot.ceac(CEAC)



# Subgroub results - work in progress

sim.runs <- 1000 

subgroups.names <- c("Male 40", "Male 60", "Male 80", "Female 40", "Female 60", "Female 80")
subgroups.n <- length(subgroups.names)


# Run model for each subgroup, and record results in an array 

simulation.subgroups <- array(data = 0, dim = c(sim.runs, 2, subgroups.n))

for(i in 1:sim.runs){
  simulation.subgroups[i,,1] <- model.THR(40,1)
  simulation.subgroups[i,,2] <- model.THR(60,1)
  simulation.subgroups[i,,3] <- model.THR(80,1)
  simulation.subgroups[i,,4] <- model.THR(40,0)
  simulation.subgroups[i,,5] <- model.THR(60,0)
  simulation.subgroups[i,,6] <- model.THR(80,0)
}


# Create a CEAC table with lambda value sequence

lambda.values <- seq(from = 0, to = 50000, by = 50)

CEAC.subgroups <- data.frame(matrix(data=NA, nrow=length(lambda.values), ncol=subgroups.n + 1))
colnames(CEAC.subgroups) <- c("lambda", subgroups.names)


# Estimate probability cost-effective for all subgroups

for (i in 1:length(lambda.values)) {

  CEAC.subgroups[i,1]<-lambda.values[i]
  CEAC.subgroups[i,2]<-pCE(lambda.values[i], simulation.subgroups[,,1])
  CEAC.subgroups[i,3]<-pCE(lambda.values[i], simulation.subgroups[,,2])
  CEAC.subgroups[i,4]<-pCE(lambda.values[i], simulation.subgroups[,,3])
  CEAC.subgroups[i,5]<-pCE(lambda.values[i], simulation.subgroups[,,4])
  CEAC.subgroups[i,6]<-pCE(lambda.values[i], simulation.subgroups[,,5])
  CEAC.subgroups[i,7]<-pCE(lambda.values[i], simulation.subgroups[,,6])

}


# Show the structure of the subgroup results 
head(CEAC.subgroups, 10)


## Need to reshape the data from wide to long to use in ggplot 
CEAC.subgroups.long <- reshape2::melt(CEAC.subgroups, id.vars = c("lambda"))
colnames(CEAC.subgroups.long) <- c("lambda", "subgroup", "pCE")


# Plots of results 

ce.plane.all(simulation.subgroups.long)

plot.ceac.all(CEAC.subgroups.long)

