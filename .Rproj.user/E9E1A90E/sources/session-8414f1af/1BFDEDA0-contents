library(caret)
data(mtcars)

#convert classification params
mtcars$am <- factor(mtcars$am,
                   levels = c(0,1),
                   labels = c("Auto", "Manual"))

train_test_split <- function(data){
  set.seed(42)
  n <- nrow(data)
  id <- sample(n, size=0.7*n)
  train_data <- data[id,]
  test_data <- data[-id,]
  return(list(train_data, test_data))
}

split_data <- train_test_split(mtcars)

#train model - linear regression
lm_model <- train(mpg ~ hp, data = split_data[[1]], method = "lm")
      
#score and evaluate - linear regression
p <- predict(lm_model, newdata=split_data[[2]])
error <- split_data[[2]]$mpg - p
rmse <- sqrt(mean(error**2))

#train model - logistics regression
glm_model <- train(am ~ mpg, data = split_data[[1]], method = "glm")

#score and evaluate - logitstics regression
p <- predict(glm_model, newdata=split_data[[2]])
acc <- mean(p == split_data[[2]]$am)


