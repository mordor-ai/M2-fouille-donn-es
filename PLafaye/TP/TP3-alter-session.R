library(bigmemory)
path <-"MODIFIER-ICI"
path <-"."
setwd(path)
xdes <- dget("dataX.desc")
dataX <- attach.big.matrix(xdes) 
dataX[1:3,4]
ind <-mwhich(dataX,4,0,"le") 
dataX[ind,4] <- 10 
mean(dataX[ind,4]) 
sd(dataX[ind,4])
dataX
dataX[ind,4] <- 11
mean(dataX[ind,4]) 
dataX[1,4] <- 500
