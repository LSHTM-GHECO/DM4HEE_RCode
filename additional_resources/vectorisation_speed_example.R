start1 <- Sys.time()

SP0.tm<-array(data=0,dim=c(5,5,60))
#  provideDimnames(SP.tm,sep="-",base=list(state.names,state.names,"time"))


for (i in 1:60) {
  SP0.tm[1,2,i]<-1-omrPTHR
  SP0.tm[1,5,i]<-omrPTHR
  SP0.tm[2,2,i]<-1-pull(tdtps[i,4])-pull(tdtps[i,..mr])
  SP0.tm[2,3,i]<-pull(tdtps[i,4])
  SP0.tm[2,5,i]<-pull(tdtps[i,..mr])
  SP0.tm[3,4,i]<-1-omrRTHR-pull(tdtps[i,..mr])
  SP0.tm[3,5,i]<-omrRTHR+pull(tdtps[i,..mr])
  SP0.tm[4,3,i]<-rrr
  SP0.tm[4,4,i]<-1-rrr-pull(tdtps[i,..mr])
  SP0.tm[4,5,i]<-pull(tdtps[i,..mr])
  SP0.tm[5,5,i]<-1
}

SP0.tm.orig <- SP0.tm

stop1 <- Sys.time()




start2 <- Sys.time()




SP0.tm<-array(data=0,dim=c(5,5,60))
#  provideDimnames(SP.tm,sep="-",base=list(state.names,state.names,"time"))


for (i in 1:60) {
  
  mortality <- pull(tdtps[i,..mr])
  
  SP0.tm[1,2,i]<-1-omrPTHR
  SP0.tm[1,5,i]<-omrPTHR
  SP0.tm[2,2,i]<-1-revision.risk.sp0[i]-mortality
  SP0.tm[2,3,i]<-revision.risk.sp0[i]
  SP0.tm[2,5,i]<-mortality
  SP0.tm[3,4,i]<-1-omrRTHR-mortality
  SP0.tm[3,5,i]<-omrRTHR+mortality
  SP0.tm[4,3,i]<-rrr
  SP0.tm[4,4,i]<-1-rrr-mortality
  SP0.tm[4,5,i]<-mortality
  SP0.tm[5,5,i]<-1
}

SP0.tm



stop2 <- Sys.time()



# No loop - just use the vectorisation within R 

start3 <- Sys.time()

SP0.tm<-array(data=0,dim=c(5,5,60))
#  provideDimnames(SP.tm,sep="-",base=list(state.names,state.names,"time"))

death.risk.vector <- pull(tdtps[,..mr])

SP0.tm[1,2,]<-1-omrPTHR
SP0.tm[1,5,]<-omrPTHR
SP0.tm[2,2,]<-1-revision.risk.sp0-death.risk.vector
SP0.tm[2,3,]<-revision.risk.sp0
SP0.tm[2,5,]<-death.risk.vector
SP0.tm[3,4,]<-1-omrRTHR-death.risk.vector
SP0.tm[3,5,]<-omrRTHR+death.risk.vector
SP0.tm[4,3,]<-rrr
SP0.tm[4,4,]<-1-rrr-death.risk.vector
SP0.tm[4,5,]<-death.risk.vector
SP0.tm[5,5,]<-1

SP0.tm.new <- SP0.tm 



stop3 <- Sys.time()


start4 <- Sys.time()

SP0.tm<-array(data=0,dim=c(5,5,60))

for (i in 1:60) {
  mortality <- as.numeric(tdtps[i,..mr])
  
  SP0.tm[1,2,i]<-1-omrPTHR
  SP0.tm[1,5,i]<-omrPTHR
  SP0.tm[2,2,i]<-1-revision.risk.sp0[i]-mortality
  SP0.tm[2,3,i]<-revision.risk.sp0[i]
  SP0.tm[2,5,i]<-mortality
  SP0.tm[3,4,i]<-1-omrRTHR-mortality
  SP0.tm[3,5,i]<-omrRTHR+mortality
  SP0.tm[4,3,i]<-rrr
  SP0.tm[4,4,i]<-1-rrr-mortality
  SP0.tm[4,5,i]<-mortality
  SP0.tm[5,5,i]<-1
}

SP0.tm.orig <- SP0.tm

stop4 <- Sys.time()


SP0.tm.orig
SP0.tm.new

check <- SP0.tm.orig - SP0.tm.new
apply(check, 3, sum)

stop1-start1
stop2-start2
stop3-start3
stop4-start4

