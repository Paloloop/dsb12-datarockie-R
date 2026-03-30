## call library
library(tidyverse)
library(caret) # classification and regression tree
library(mlbench) # collection of ml datasets

## house price prediction
data("BostonHousing")
df <- BostonHousing

## 0.check if any missing values
#ถ้าไม่มี NA จะเป็น true ทั้งหมด แล้วเฉลี่ยออกมาเป็น 1 ถ้าไม่เท่าแสดงว่ามี NA
b <- complete.cases(df)

## 1.split data
set.seed(42)
n <- nrow(df)
train_id <- sample(1:n, size=0.8*n)
train_df <- df[train_id,]
test_df <- df[-train_id,]

## 2.train model

## change resampling method
#train_ctrl <- trainControl(
#  method = "boot",
#  number = 25 )

#train_ctrl <- trainControl(
#  method = "LOOCV")

#train_ctrl <- trainControl(
#  method = "cv", #k-fold
#  number = 5,)

train_ctrl <- trainControl(
  method = "repeatedcv", #repeated k-fold
  number = 5,
  repeats = 5 #ให้ทำซ้ำอีก 5 ครั้ง model จะ train ทั้งหมด 25
)
# medv = f(crim, rm, age)
model <- train(medv ~ .,
               data = train_df, 
               method = "rf",
               trControl = train_ctrl)
#model <- train(medv ~ crim + rm + age,
#               data = train_df, 
#               method = "lm",
#              trControl = train_ctrl)

## 3.score model
p_medv <- predict(model, newdata = test_df)

## 4.evaluate model
#compare RMSE of train vs. test data
train_rmse <- model$results$RMSE
#> model$results
#intercept     RMSE  Rsquared     MAE    RMSESD RsquaredSD     MAESD
#1      TRUE 6.059216 0.5506949 3.94915 0.8371703  0.1141004 0.3722899

sq_error  <- sqrt(mean((p_medv - test_df$medv)**2))
#summary : lm good fit > 6.059216 vs 6.289014, rf is excellent 3.507717 vs 3.141237





