library(tidyverse)
library(mlbench)
library(caret)

## random forest
## clean data
df <- PimaIndiansDiabetes %>% drop_na()
mean(complete.cases(df)) == 1

## split data
set.seed(42)
n <- nrow(df)
id <- sample(1:n, size=0.8*n)
train_data <- df[id,]
test_data <- df[-id,]

## train model
set.seed(42)
ctrl <- trainControl(method = "cv",
                     number = 5,
                     verboseIter = TRUE)

mtry.grid <- data.frame(mtry = c(3,4,5))

model <- train(diabetes ~.,
               data = train_data,
               method = "rf",
               trControl = ctrl,
               tuneGrid = mtry.grid)

## score and evaluate
p <- predict(model, newdata = test_data)
mean(p == test_data$diabetes) #0.7857143


