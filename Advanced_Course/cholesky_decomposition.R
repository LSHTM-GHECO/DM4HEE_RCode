#  Decision Modelling for Health Economic Evaluation
#  Cholesky Decomposition by hanD
#  Authors: Nichola Naylor & Jack Williams

### !!! set working directory as the folder this is stored in
## added this in to allow for the running of instruction pdf knitting
## whilst reading in data from the same subfolder
## students can ignore if not re-knitting the pdfs, just make sure data files
## are stored in the same file as template/solution files

# require("rstudioapi")  
# setwd(dirname(getActiveDocumentContext()$path)) # Set working directory to source file 


## importing data.table library
library(data.table) ## install this package if not already installed before continuing

#  Read in the lcovariance table
hazards <- read.csv("hazardfunction.csv", header=TRUE) ## importing the hazard inputs from the regression analysis

cov.55<-read.csv("cov55.csv",row.names=1,header=TRUE) ## importing the 

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
  
  # ### a note of a more efficient (but less visual) way
  # cholm[1,1] <- sqrt(cov.55[1,1])
  # cholm[2:5,1] <- cov.55[2:5,1]/cholm[1,1]
  # cholm[2,2] <- sqrt(cov.55[2,2]-(cholm[2,1]^2))
  # cholm[3:5,2] <- (cov.55[3:5,2]-cholm[2,1]*cholm[3:5,1])/cholm[2,2]

  