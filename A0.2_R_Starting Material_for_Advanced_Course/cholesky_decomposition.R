#  DM4HEE 
#  Cholesky Decomposition by hand
#  Author: Nichola Naylor
#  Date created: 22 February 2021
#  Date last edit: 12 March 2021


library(data.table)

#  Read in the lcovariance table
hazards <- read.csv("inputs/hazardfunction.csv", header=TRUE) ## importing the hazard inputs from the regression analysis

cov.55<-read.csv("inputs/cov55.csv",row.names=1,header=TRUE) ## importing the 

## Coefficients - on the log hazard scale
mn.lngamma <- hazards$coefficient[1] ## Ancilliary parameter in Weibull distribution - equivalent to lngamma coefficient
mn.cons <- hazards$coefficient[2] ##Constant in survival analysis for baseline hazard
mn.ageC <- hazards$coefficient[3] ## Age coefficient in survival analysis for baseline hazard
mn.maleC <- hazards$coefficient[4] ## Male coefficient in survival analysis for baseline hazard
mn.NP1 <- hazards$coefficient[5]
 
mn<-c(mn.lngamma, mn.cons,mn.ageC,mn.maleC,mn.NP1)
  
  
  var.names <- c("lngamma","cons","age","male","NP1")
  n.var <- length(var.names)
  
  cholm <-  matrix(data=0, nrow=n.var, ncol=n.var,
                   dimnames = list(var.names, var.names))
  
  cholm[1,1] <- sqrt(cov.55[1,1])
  cholm[2,1] <- cov.55[2,1]/cholm[1,1]
  cholm[3,1] <- cov.55[3,1]/cholm[1,1]
  cholm[4,1] <- cov.55[4,1]/cholm[1,1]
  cholm[5,1] <- cov.55[5,1]/cholm[1,1]
  
  cholm[2,2] <- sqrt(cov.55[2,2]-(cholm[2,1]^2))
  cholm[3,2] <- (cov.55[3,2]-cholm[2,1]*cholm[3,1])/cholm[2,2]
  cholm[4,2] <- (cov.55[4,2]-cholm[2,1]*cholm[4,1])/cholm[2,2]
  cholm[5,2] <- (cov.55[5,2]-cholm[2,1]*cholm[5,1])/cholm[2,2]
  
  cholm[3,3] <- sqrt(cov.55[3,3]-(cholm[3,1]^2)-(cholm[3,2]^2))
  cholm[4,3] <- (cov.55[4,3]-cholm[3,1]*cholm[4,1]-cholm[3,2]*cholm[4,2])/cholm[3,3]
  cholm[5,3] <- (cov.55[5,3]-cholm[3,1]*cholm[5,1]-cholm[3,2]*cholm[5,2])/cholm[3,3]
  
  cholm[4,4] <- sqrt(cov.55[4,4]-(cholm[4,1]^2)-(cholm[4,2]^2)-(cholm[4,3]^2))
  cholm[5,4] <- (cov.55[5,4]-cholm[4,1]*cholm[5,1]-cholm[4,2]*cholm[5,2]-cholm[4,3]*cholm[5,3])/cholm[4,4]
  
  cholm[5,5] <- sqrt(cov.55[5,5]-(cholm[5,1]^2)-(cholm[5,2]^2)-
                       (cholm[5,3]^2)-(cholm[5,4]^2))
  
  # ### a note of a more efficient way
  # cholm[1,1] <- sqrt(cov.55[1,1])
  # cholm[2:5,1] <- cov.55[2:5,1]/cholm[1,1]
  # cholm[2,2] <- sqrt(cov.55[2,2]-(cholm[2,1]^2))
  # cholm[3:5,2] <- (cov.55[3:5,2]-cholm[2,1]*cholm[3:5,1])/cholm[2,2]
  # ### can be expanded
  