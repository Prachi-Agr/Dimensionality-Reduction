#FLD on Glass dataset
library(tidyverse)
library(factoextra)
library(MASS)
library(caTools) 
library("car")
library("caret")
Data <- read_csv("glass.csv")
set.seed(123) 
training.samples <- Data$Type %>%
  createDataPartition(p = 0.9, list = FALSE)
train.data <- Data[training.samples[1:194], ]
test.data <- Data[-training.samples, ]
model <- lda(Type ~., data = train.data)
model
predictions <- model %>% predict(test.data)
mean(predictions$class==test.data$Type)
lda.data <- cbind(train.data, predict(model)$x)
ggplot(lda.data, aes(LD1, LD2)) +
  geom_point(aes(color = Type))
mean(predictions$class==test.data$Type)