#  DM4HEE 
#  Exercise 5.7 - Analysing THR model simulation results
#  Author: Andrew Briggs
#  Date created: 22 February 2021
#  Date last edit: 22 February 2021

library(dplyr)

#  Make sure you have already created the function: model.THR(age,male)


#Note to self - is there a way to use 'replicate' and convert to dataframe to avoid using'rdply' from the 'plyr' library?
#Problem is that replicate gives n*2d vectors and creating a dataframe generates 2rows and n columns
#Solution: use t() to transpose before making into a data.frame
#Using system.time() to check the two functions reveals slight gain for avoiding 'rdply' but no opportunity to use option '.progress="text"'

sim.runs<-1000
simulation.results<-data.frame(t(replicate(sim.runs,model.THR(60,0))))

#system.time(simulation.results<-rdply(100,model.THR(60,0),.id=NULL))
colnames(simulation.results)<-c("inc.QALYs","inc.costs")
plot(simulation.results$inc.QALYs,simulation.results$inc.cost)



pCE<-function(lambda) {
  simulation.results$nmb<-simulation.results$inc.QALYs*lambda-simulation.results$inc.costs
  simulation.results$CE<-simulation.results$nmb>0
  summary(simulation.results$CE)
  probCE<-sum(simulation.results$CE)/sim.runs
  return(probCE)
}


CEAC<-matrix(data=NA,nrow=1000,ncol=2)
for (lambda in 1:1000) {
  CEAC[lambda,1]<-lambda
  CEAC[lambda,2]<-pCE(lambda)
}
CEAC<-data.frame(data=CEAC)
colnames(CEAC)<-c("lambda","pCE")
plot(CEAC$lambda,CEAC$pCE, type="l")


