library(bigmemory)
setwd("/Users/emmanuelpellegrin/git/MIASHS/M2/M2-fouille-donn-es/PLafaye/TP")

system.time(X <- read.big.matrix("./Data/X_3000_1340000_200_logistic-short.txt",
                                 sep=",",backingfile ="dataX.bin",
                                 descriptor = "dataX.desc", type="double",shared=TRUE))
# utilisateur     système      écoulé 
# 10.098       0.310      10.564 

# I had to modify the dirname value in the file 'dataX.desc'
dataXdesc <- dget("./Data/dataX.desc") 
system.time(dataX <- attach.big.matrix(dataXdesc)) ## user system elapsed
## 0.001 0.000 0.001
gc()
## Ncells 553079 29.6 1237667 66.1 675736 36.1 ## Vcells 1016466 7.8 8388608 64.0 1753842 13.4
# used (Mb) gc trigger  (Mb) limit (Mb) max used  (Mb)
# Ncells 1452401 77.6    4585281 244.9         NA  3085985 164.9
# Vcells 4483668 34.3   14577142 111.3      16384 11495532  87.8
dim(dataX)
## [1] 3000 1340000
dataX
## An object of class "big.matrix" ## Slot "address":
## <pointer: 0x5567db2e05e0>
is.filebacked(dataX) ## [1] TRUE 
is.big.matrix(dataX) ## [1] TRUE 
is.shared(dataX)
## [1] TRUE

summary(dataX[, 1:2]) ##V1 V2
## Min. :-3.53959
## 1st Qu.:-0.66469
## Median :-0.02253
## Mean :-0.00420
## 3rd Qu.: 0.71158
## Max. : 3.81028
## Min. :-3.671300 1st Qu.:-0.672130 Median :-0.019991 Mean :-0.005014 3rd Qu.: 0.659005 Max. : 3.064524
range(dataX[,1], na.rm = TRUE) 
## [1] -3.539586 3.810277
describe(dataX)
dataX

tail(dataX, 1)
max(dataX[,])

indices <- mwhich(dataX, 1, -0.66469, "le") 
length(indices)
## [1] 750
length(indices)/dim(dataX)[1]
## [1] 0.25

dataX
## An object of class "big.matrix" ## Slot "address":
## <pointer: 0x5567db2e05e0>
dataX[1,1]
## [1] -0.6264538
dataXcopy <- dataX
dataXcopy
## An object of class "big.matrix" ## Slot "address":
## <pointer: 0x5567db2e05e0>
# dataX et dataXcopy pointent vers les mêmes données en memory.
# Nous souslignons l’utilisation de deepcopy():
  dataX[1,1]
## [1] -0.6264538 dataX[1,1]<- as.integer(1) dataX[1,1]
## [1] 1
dataXcopy[1,1] ## [1] 1
w <- deepcopy(dataXcopy,1:10,backingfile = "w.bin",descriptorfile = "w.desc") 
dim(w)
## [1] 3000 10
w
## An object of class "big.matrix" ## Slot "address":
## <pointer: 0x5567df6ec690>
# 4
# Traiter des big data en R avec le package bigmemory
# Nous exportons aussi cette “bigmatrix” dans un fichier texte pour utilisation ultérieure.
write.big.matrix(w,"w.txt")


dataX[1:3,1:3]
## [,1] [,2] [,3] ## [1,] 1.0000000 0.7391149 -0.6188271 ## [2,] 0.1836433 0.3866087 -1.1094220 ## [3,] -0.8356286 1.2963972 -2.1703352
dataX[,1] <- 1
dataX[1:3,1:3]
## [,1] [,2] [,3]
## [1,]
## [2,]
## [3,]
# 1 0.7391149 -0.6188271 1 0.3866087 -1.1094220 1 1.2963972 -2.1703352
# Le fichier   n’a pas encore été changé.
# Vous devez utiliser flush()
flush(dataX)
newdataX <- attach.big.matrix(dget("./Data/dataX.desc")) 
newdataX[1:3,1:3]
##
## [1,]
## [2,]
## [3,]

dataX[1:3,4]
## [1] -1.2171201 -0.9462293 0.0914098 
ind <-mwhich(dataX,4,0,"le") 
mean(dataX[ind,4])
## [1] -0.7828649
sd(dataX[ind,4])
## [1] 0.5866901
dataX

mean(dataX[ind,4]) 
sd(dataX[ind,4])
dataX[1,4] 



install.packages("biglm")
library(biglm)
airquality
big.model = biglm(Temp ~ Wind + Solar.R + Ozone, data = airquality) 
summary(big.model)
model = lm(Temp ~ Wind + Solar.R + Ozone, data = airquality) 
summary(model)[4]


y <- read.big.matrix("./Data/y_3000_1340000_200_logistic.txt", type = "integer", backingfile = "y.bin", descriptorfile = "y.desc")
set.seed(1)
dim(dataX)
indp <- sample(1:(dim(dataX)[2]),300)
X.cut <- dataX[,indp]
data.cut <- data.frame(cbind(Y=as.vector(y[,1]),X.cut))

model2 <- bigglm(Y~V2+V3,data=data.cut,family=binomial("logit")) 
summary(model2)


#
#
#
# user system elapsed # 89.464 3.471 96.530 # dim(x)
system.time(x <- read.big.matrix("./Data/Netflix.txt", sep = ",", type = "integer",
                                 backingfile = "Netflix.bin", descriptor = "Netflix.desc",
shared = TRUE, col.names = c("movie", "customer", "rating","year", "month")))
dim(x)
##Combien de films ? Combien de clients ?

range(x[,1])
## [1] 1 17770 range(x[,2])
## [1] 6 2649429 summary(x[,3])
## Min. 1st Qu. Median ## 1.000 3.000 4.000 range(x[,4])
## [1] 1999 2005

install.packages("biganalytics")
library(biganalytics)
summary(x)
##

cust.indices <- mwhich(x, "customer", 2442, "eq") 
head(x[cust.indices, ])

# • Des comparaisons plus complexes: par exemple, nous pourrions être intéressés au films du clien 2442’s qui ont été notés 2 ou pire entre février et octobre 2004:
these <- mwhich(x, c("customer", "year", "month", "rating"), 
                list(2442, 2004, c(2, 10), 2), 
                list("eq", "eq", c("ge", "le"),"le"), "AND")
x[these, ]



mnames <- read.csv("Data/movie_titles.csv", header = FALSE) 
names(mnames) <- c("movie", "year", "Name of Movie") 
mnames[mnames[, 1] %in% unique(x[these, 1]), c(1, 3)]






lm.0 <- biglm.big.matrix(rating ~ year, data = x, fc = "year") 
print(summary(lm.0)$mat)





library(parallel)
cores <- detectCores() ## Combien de coeurs avons nous? 
print(cores)
## [1] 4
one.simu <- function(i) {
  ## tracer les données
  n <- 1000; p <- 500
  x <- matrix(rnorm(n*p),n,p) ; y <- rnorm(n) 
  ## renvoie les coefficients ridge
  train <- 1:floor(n/2)
  test <- setdiff(1:n,train)
  ridge <- lm.ridge(y~x+0,lambda=1,subset=train) err <- (y[test] - x[test, ] %*% ridge$coef )^2 
  return (list(err = mean(err), sd = sd(err)))
}
library(MASS)
out <- mclapply(1:8, one.simu, mc.cores=cores) head(do.call(rbind, out))








readchunk <- function(X, g, size.chunk){
  rows <- ((g - 1) * size.chunk + 1):(g * size.chunk) 
  chunk <- X[rows,]
}

cpc <- function(X, Y, ng = 1){
  readchunk <- function(X, g, size.chunk) {
    rows <- ((g - 1) * size.chunk + 1):(g * size.chunk) 
    chunk <- X[rows,]
  }
  res <- foreach(g = 1:ng, .combine = "+") %dopar% {
    size.chunk <- nrow(X) / ng
    chunk.X <- readchunk(X, g, size.chunk) 
    chunk.Y <- readchunk(Y, g, size.chunk) 
    term <- t(chunk.X) %*% chunk.Y
  }
  return(res)
}

set.seed(1) 
n <- 10000 
p <- 4
q <- 3
X <- matrix(rnorm(n*p),ncol=4,nrow=n)
Y <- matrix(rnorm(n*q),ncol=3,nrow=n)

res <- crossprod(X,Y) 
res
install.packages("doSNOW")
library(doSNOW)
cl <- makeCluster(4) 
registerDoSNOW(cl) 
res.cpc <- cpc(X,Y,ng=10) 
stopCluster(cl)
res.cpc




dataX <- read.big.matrix("./Data/MNIST10000.csv",descriptorfile = "MNIST.des",backingfile = "MNIST.bin",
                         header = TRUE, sep = ";", type = "integer")
Xdes <- describe(dataX)                         

Xdes



XtX.big <- function(X.des, ng = 1) {
  readchunk <- function(X, g, size.chunk) {
    rows <- ((g - 1) * size.chunk + 1):(g * size.chunk) 
    chunk <- X[rows,]
  }
  res <- foreach(g = 1:ng, .combine = "+") %dopar% {
    require("bigmemory")
    X <- attach.big.matrix(X.des) 
    size.chunk <- nrow(X) / ng
    chunk.X <- readchunk(X, g, size.chunk)
    term <- t(chunk.X) %*% chunk.X 
    }
  return(res)
}

library(doSNOW)
cl <- makeCluster(4)
registerDoSNOW(cl)
cl <- makeCluster(4)
res.big <- XtX.big(Xdes,ng=10)
stopCluster(cl)
summary(res.big[,101])
head(res.big)

