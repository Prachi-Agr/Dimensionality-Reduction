library(tidyverse)
# NCA
nca <- function( x
                 , labels
                 , A_init = diag(ncol(x))
                 , N_iter=1e2
                 , learning_rate = 0.01
){
  x <- as.matrix(x)
  #labels <- as.factor(labels)
  
  A <- A_init
  
  N <- nrow(x)
  stopifnot(NROW(x) == length(labels))
  
  p <- numeric(N)
  p_cum <- numeric(N_iter)
  for (it in seq_len(N_iter)){
    for (i in seq_len(N)){
      # softmax, with LOO
      D <- tcrossprod(A, x)       # (dA, N)
      D <- (D - as.numeric(D[,i]))
      p_ik <- exp(-colSums(D*D))       # (N)
      
      p_ik[i] <- 0
      softmax <- sum(p_ik)
      if (softmax > .Machine$double.eps){
        p_ik <- p_ik/sum(p_ik)             # (N)
      }
      # end softmax
      
      # neighbors that predict the correct label
      correct_label <- labels == labels[i]  # (N)
      
      p[i] <- sum(p_ik[correct_label])
      d    <- t(t(x) - as.numeric(x[i,]))  # (N, dx)
      pd <- p_ik * d                    # (N, dx)
      
      g <- (p[i]*crossprod(d, pd)) - crossprod(d[correct_label,], pd[correct_label,]) # (dx, dx)
      A <- A + learning_rate * (A %*% g) # (dx, dA)

    }
    p_cum[it] <- sum(p)
  }
  
  list( A = A
        , p = p
        , A_norm = A/A[1,1]
        , p_cum=p_cum
  )
}

scaling <- function(x){
  x_min <- apply(x, 2, min)
  x <- sweep(x, 1, x_min)
  x_max <- apply(x, 2, max)
  diag(1/(x_max))
}

#Applying NCA
x <- iris[1:4]
x <- as.matrix(x)
labels <- iris[[5]]
A <- diag(ncol(x))
A <- scaling(x)
result <- nca(x=x, labels = labels, A_init = A, N_iter = 200, learning_rate = 1e-2)
result$A_norm
#
# # 2d projection
x_2d <- t(tcrossprod(result$A, x))
x_2d <- as.data.frame(x_2d)

plot(x_2d, col=iris$Species)

Species=iris[5]
new=cbind(Species,x_2d)

#Splitting data into training and test set
install.packages('caTools') 
library(caTools) 

set.seed(123) 
split = sample.split(new, SplitRatio = 0.75) 

training_set = subset(new, split == TRUE) 
test_set = subset(new, split == FALSE) 

train.data=training_set[,1:5]
test.data=test_set[,2:5]
y=test_set[1]

#Using decision tree for classification
install.packages("rpart")
library(rpart)
rpart.model <- rpart(Species ~ .,data = train.data, method = "class")
rpart.model

rpart.prediction <- predict(rpart.model, test.data, type='class')

#Confusion Matrix
confMat <- table(test_set$Species,rpart.prediction)
confMat
#Accuracy
sum(diag(confMat))/sum(confMat)


