## load library
library(tidyverse)
library(caret)
library(mlbench)

## predict diabetes
# 0.clean missing values
df <- PimaIndiansDiabetes %>%
  drop_na()

# check data
is_cleaned <- mean(complete.cases(df))==1

# 1.split data
set.seed(42)
n <- nrow(df) 
id <- sample(1:n, size=n*0.8)
train_df <- df[id,]
test_df <- df[-id,]

# 2.train model
ctrl <- trainControl(method = "cv", #k-fold resampling
                     number = 5,
                     verboseIter = TRUE,)

k_grid <- data.frame(k = c(3,5,7,9))
model <- train(diabetes ~ glucose + pressure + age + mass,
               data = train_df,
               method = "knn",
               metric = "Accuracy", 
               trControl = ctrl,
               tuneGrid = k_grid, # customize k
               preProcess = c("center","scale")) # standardization
              #preProcess = c("range") #normalization

#metrics
#Kappa ยิ่งเยอะยิ่งดี ใช้กับ binary classification ที่เป็น imbalance pos/neg != 50:50
#ROC ยิ่งเข้าใกล้ 1 ยิ่งดี
#Accuracy
#RMSE
#AUC (area under the curve)

