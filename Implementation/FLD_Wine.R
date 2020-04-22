#FLD on Wine dataset
library("car")
library(tidyverse)
library(caret)
library(MASS)
data(wine,package = 'rattle')
set.seed(123)
training.samples <- wine$Type %>%
  createDataPartition(p = 0.75, list = FALSE)
train.data <- wine[training.samples, ]
test.data <- wine[-training.samples, ]
preproc.parameter <- train.data %>% 
  preProcess(method = c("center", "scale"))
train.transformed <- preproc.parameter %>% predict(train.data)
test.transformed <- preproc.parameter %>% predict(test.data)
model <- lda(Type ~., data = train.transformed)
predictions <- model %>% predict(test.transformed)
lda.data <- cbind(train.transformed, predict(model)$x)
ggplot(lda.data, aes(LD1, LD2)) +
  geom_point(aes(color = Type))
model
mean(predictions$class==test.transformed$Type)