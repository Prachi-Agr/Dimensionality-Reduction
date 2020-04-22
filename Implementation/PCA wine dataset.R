library(tidyverse) 
install.packages("factoextra")
library(factoextra)
#Read dataset
Data <- read_csv("../input/wine_new.csv")
#Split data into training and test set
install.packages('caTools') 
library(caTools) 

set.seed(123) 
split = sample.split(Data, SplitRatio = 0.75) 

training_set = subset(Data, split == TRUE) 
test_set = subset(Data, split == FALSE)
#Applying PCA 
W.pca <- princomp(training_set[,-1], cor = TRUE, scores = TRUE, covmat = NULL)
summary(W.pca)
W.pca$loadings
#Scree plots
plot(W.pca)
fviz_eig(W.pca)

#Cumulative scree plot
std_dev=W.pca$sde
pr_var <- std_dev^2
prop_varex <- pr_var/sum(pr_var)
plot(cumsum(prop_varex), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")

fviz_pca_ind(W.pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

fviz_pca_var(W.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

fviz_pca_biplot(W.pca, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)

train.data <- data.frame(Wine=training_set$Wine, W.pca$score)
train.data <- train.data[,1:3]
train.data
test_set[1]


#Using decision tree to classify
install.packages("rpart")
library(rpart)
rpart.model <- rpart(Wine ~ .,data = train.data, method = "class")
rpart.model

#transform test into PCA
test.data <- predict(W.pca, newdata = test_set[,2:14])
test.data <- as.data.frame(test.data)

test.data <- test.data[,1:2]

#make prediction on test data
rpart.prediction <- predict(rpart.model, test.data, type='class')
rpart.prediction

#Confusion Matrix
confMat <- table(test_set$Wine,rpart.prediction)
confMat

sum(diag(confMat))/sum(confMat)

#Classifying without dimensionality reduction
library(rpart)
rpart.model <- rpart(Wine ~ .,data = training_set, method = "class")
rpart.model

#make prediction on test data
rpart.prediction <- predict(rpart.model, test_set[,1:14],type='class')
confMat <- table(test_set$Wine,rpart.prediction)
confMat

sum(diag(confMat))/sum(confMat)
