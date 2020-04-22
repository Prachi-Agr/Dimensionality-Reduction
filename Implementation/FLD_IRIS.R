#FLD on Iris dataset
library(tidyverse)
library(caret)
library(MASS)
data("iris")
set.seed(123)
training.samples <- iris$Species %>%
  createDataPartition(p = 0.75, list = FALSE)
train.data <- iris[training.samples, ]
test.data <- iris[-training.samples, ]
preproc.param <- train.data %>% 
  preProcess(method = c("center", "scale"))
train.transformed <- preproc.param %>% predict(train.data)
test.transformed <- preproc.param %>% predict(test.data)
model <- lda(Species~., data = train.transformed)
predictions <- model %>% predict(test.transformed)
model
mean(predictions$class==test.transformed$Species)
lda.data <- cbind(train.transformed, predict(model)$x)
ggplot(lda.data, aes(LD1, LD2)) +
  geom_point(aes(color = Species))
mean(predictions$class==test.transformed$Species)