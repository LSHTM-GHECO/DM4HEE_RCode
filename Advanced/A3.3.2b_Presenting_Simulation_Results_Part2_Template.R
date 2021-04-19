#  Decision Modelling for Health Economic Evaluation
#  Advanced Course Exercise 3b (Part 2): TEMPLATE FILE
#  Authors: Andrew Briggs, Jack Williams & Nichola Naylor

### Loading useful packages
library(data.table)
library(tidyr)
library(dplyr)
library(ggplot2)
library(reshape2) 

#  Reading the data needed from csv files
hazards <- read.csv("Advanced/A0.2_R_Starting Material_for_Advanced_Course/hazardfunction.csv", header=TRUE) ## importing the hazard inputs from the regression analysis
cov.55 <- read.csv("Advanced/A0.2_R_Starting Material_for_Advanced_Course/cov55_NP2.csv",row.names=1,header=TRUE) ## importing the covariance matrix
life.table <- read.csv("Advanced/A0.2_R_Starting Material_for_Advanced_Course/life-table.csv", header=TRUE)
life.table<- as.data.table(life.table)

####***** THR MODEL FUNCTION ****#####

#########**** PARAMETERS *****######


