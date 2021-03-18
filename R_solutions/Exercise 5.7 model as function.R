#  DM4HEE 
#  Exercise 5.7 - Analysing THR model simulation results
#  Author: Andrew Briggs
#  Date created: 22 February 2021
#  Date last edit: 15 March 2021

library(dplyr)

#  Make sure you have already created the function: model.THR(age,male)

# There needs to be a way to call and run the other model - one option may be to have the function in a seperate sheet (to call)
# source("R_solutions/Exercise 4.8 with sim.R")

#Note to self - is there a way to use 'replicate' and convert to dataframe to avoid using'rdply' from the 'plyr' library?
#Problem is that replicate gives n*2d vectors and creating a dataframe generates 2rows and n columns
#Solution: use t() to transpose before making into a data.frame
#Using system.time() to check the two functions reveals slight gain for avoiding 'rdply' but no opportunity to use option '.progress="text"'


# JW: there are manual ways to add the progress bar - i've added to 4.8 with Sim

sim.runs<-100

# Andy option
simulation.results<-data.frame(t(replicate(sim.runs,model.THR(60,0))))


# Jack alternative without dpylr
simulation.results <- data.frame(matrix(data = 0, sim.runs, 2))
for(i in 1:sim.runs)   simulation.results[i,] <- model.THR(60,0)


#system.time(simulation.results<-rdply(100,model.THR(60,0),.id=NULL))
colnames(simulation.results)<-c("inc.QALYs","inc.costs")
plot(simulation.results$inc.QALYs,simulation.results$inc.cost)



pCE<-function(lambda, results = simulation.results) {

  nmb <- results[,1]*lambda - results[,2] 
  CE <- nmb>0
  # summary(results$CE) # why is this needed in the function? it won't be seen... 
  probCE<- mean(CE) # more flexible 
  return(probCE)
  
}


CEAC<-data.frame(matrix(data=NA,nrow=1000,ncol=2))
for (lambda in 1:1000) {
  CEAC[lambda,1]<-lambda
  CEAC[lambda,2]<-pCE(lambda, simulation.results)
}

colnames(CEAC)<-c("lambda","pCE")

plot(CEAC$lambda,CEAC$pCE, type="l")





# Subgroub results - work in progress
# 
# 
# subgroups.names <- c("M40", "M60", "M80", "F40", "F60", "F80")
# subgroups.n <- length(subgroups.names)
# 
# 
# 
# simulation.subgroups <- array(data = 0, dim = c(sim.runs, 2, subgroups.n))
# 
# 
# for(i in 1:sim.runs){
#   simulation.subgroups[i,,1] <- model.THR(40,0)
#   simulation.subgroups[i,,2] <- model.THR(60,0)
#   simulation.subgroups[i,,3] <- model.THR(80,0)
#   simulation.subgroups[i,,4] <- model.THR(40,1)
#   simulation.subgroups[i,,5] <- model.THR(60,1)
#   simulation.subgroups[i,,6] <- model.THR(80,1)
# }   
# 
# 
# CEAC.subgroups <- data.frame(matrix(data=NA, nrow=1000, ncol=subgroups.n + 1))
# colnames(CEAC.subgroups) <- c("lambda", subgroups.names)
# 
# pCE(lambda, simulation.subgroup[,,1])
# 
# for (lambda in 1:1000) {
#   
#   CEAC.subgroups[lambda,1]<-lambda
#   CEAC.subgroups[lambda,2]<-pCE(lambda, simulation.subgroups[,,1])
#   CEAC.subgroups[lambda,3]<-pCE(lambda, simulation.subgroups[,,2])
#   CEAC.subgroups[lambda,4]<-pCE(lambda, simulation.subgroups[,,3])
#   CEAC.subgroups[lambda,5]<-pCE(lambda, simulation.subgroups[,,4])
#   CEAC.subgroups[lambda,6]<-pCE(lambda, simulation.subgroups[,,5])
#   CEAC.subgroups[lambda,7]<-pCE(lambda, simulation.subgroups[,,6])
# 
# }
# 
# 
# 
# 
# # need to consider the base R plot
# 

