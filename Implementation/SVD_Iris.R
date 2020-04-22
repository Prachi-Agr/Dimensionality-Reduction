library(tidyverse)
dat <- read_csv("../input/iris.csv")

dat<-as.matrix(dat)
svd.mod <- svd(dat)
U <- dat %*%  svd.mod$v  %*% diag(1./svd.mod$d)\

U.reduced <-dat %*% svd.mod$v[,1:7,drop=FALSE] %*% diag((svd.mod$d)[1:7,drop=FALSE])
typeof(U.reduced)
U.reduced

dimReduce <- function(x, k=floor(ncol(x)/2), supplemental.cols=NULL) {
  colIdxs <- which(colnames(x) %in% supplemental.cols)
  colNames <- names(x[,-colIdxs])
  sol <- svd(x[,-colIdxs])
  sol.U <- as.matrix(x[,-colIdxs]) %*% (sol$v)[,1:k,drop=FALSE] %*% 
    diag((sol$d)[1:k,drop=FALSE])
  sol.U = sol.U@data
  res <- cbind(sol.U,x[,colIdxs,drop=FALSE])
  names(res) <- c(names(sol.U@data),names(x[,colIdxs]))
  res
}

dat <- read_csv("../input/iris.csv")
dat$ID <- seq_len(nrow(dat))
ore.drop("IRIS2")
ore.create(dat,table="IRIS2")
row.names(IRIS2) <- IRIS2$ID
IRIS2[1:5,]

IRIS2.reduced <- dimReduce(IRIS2, 2, supplemental.cols=c("ID","Species"))
dim(IRIS2.reduced) 

#Decision tree classifier
library(rpart)
m1 <- rpart(Species~.,iris)
res1 <- predict(m1,iris,type="class")
table(res1,iris$Species)
dat2 <- ore.pull(IRIS2.reduced)
m2 <- rpart(Species~.-ID,dat2)
res2 <- predict(m2,dat2,type="class")
table(res2,iris$Species)

m2.1 <- ore.odmDT(Species~.-ID, IRIS2.reduced)
res2.1 <- predict(m2.1,IRIS2.reduced,type="class",supplemental.cols = "Species")
table(res2.1$PREDICTION, res2.1$Species)