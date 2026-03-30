## Logistics Regression Example

happiness <- c(10, 8, 9, 7, 8, 5, 9, 6, 8, 7, 1, 1, 3, 1, 4, 5, 6, 3, 2, 0)
divorce <- c(0, 0, 0, 0, 0 ,0 ,0 ,0 ,0 ,0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)

df <- data.frame(happiness, divorce)

## Fit Logistics Regression Full Dataset
model <- glm(divorce ~ happiness, data = df, family = "binomial")

summary(model)

## Predict and Evaluate Model
df$prob_divorce <- predict(model, type="response")
df$pred_divorce <- if_else(df$prob_divorce >= 0.5, 1,0)

## confusion matrix
conM <- table(df$pred_divorce, df$divorce, 
      dnn = c("Predicted", "Actual"))
#         Actual
#Predicted 0 1
#        0 9 1
#        1 1 9

## Model Evakuation
acc <- (conM[1,1]+conM[2,2])/sum(conM)
precision <- (conM[2,2])/(conM[2,1]+conM[2,2])
recall <- (conM[2,2])/(conM[1,2]+conM[2,2])
f1 <- (2*precision*recall)/(precision+recall)

cat("Accuracy:",acc,"\nPrecision:",precision,"\nRecall:",recall,"\nF1 Score:",f1)

## Homework
library(titanic)
head(titanic_train)

titanic_train <- na.omit(titanic_train)
titanic_train$SexFactor <- factor(titanic_train$Sex,
                                  levels = )
nrow(titanic_train)

set.seed(42)
n <- nrow(titanic_train)
id <- sample(1:n, size=n*0.7) #70% train, 30% test
train_data <- titanic_train[id, ]
test_data <- titanic_train[-id, ]
#train
titanic_model <- glm(Survived ~ Pclass + Age + Sex, data = train_data, family = "binomial")
train_data$prob_survive <- predict(titanic_model, type = "response")
train_data$pred_survive <- if_else(train_data$prob_survive >= 0.5, 1,0)
ti_conM <- table(train_data$pred_survive, train_data$Survived, 
              dnn = c("Prediction","Actual"))

ti_accuracy <- (ti_conM[1,1]+ti_conM[2,2])/sum(ti_conM)

#test
test_data$prob_survive <- predict(titanic_model, newdata = test_data, type = "response")
test_data$pred_survive <- if_else(test_data$prob_survive >= 0.5,1,0)
ti_test_conM <- table(test_data$pred_survive, test_data$Survived, 
                      dnn = c("Prediction","Actual"))
ti_test_acc <-(ti_test_conM[1,1]+ti_test_conM[2,2])/sum(ti_test_conM) 


