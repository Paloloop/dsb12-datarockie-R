## Correlation
cor(mtcars$hp, mtcars$mpg) #-0.7761684

plot(mtcars$hp, mtcars$mpg, pch=16)
plot(mtcars$wt, mtcars$mpg, pch=16)
plot(mtcars$wt, mtcars$hp, pch=16)

cor(mtcars[,c("mpg","wt","hp")])
## dplyr (tidyverse)
library(dplyr)
mtcars %>%
  select(mpg,wt,hp,am) %>% 
  cor(.)

## significant test
cor.test(mtcars$hp, mtcars$mpg)
#Pearson's product-moment correlation
#
#data:  mtcars$hp and mtcars$mpg
#t = -6.7424, df = 30, p-value = 1.788e-07
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# -0.8852686 -0.5860994
#sample estimates:
#       cor 
#-0.7761684

## Linear Regression
## mpg = f(hp)

lmFit <- lm(mpg ~ hp, data = mtcars)
#Coefficients:
#  (Intercept)           hp  
#30.09886     -0.06823  

summary(lmFit)
#Residuals:
#  Min      1Q  Median      3Q     Max 
#-5.7121 -2.1122 -0.8854  1.5819  8.2360 
#
#Coefficients:
#             Estimate    Std.Error  t value Pr(>|t|)    
#(Intercept)   30.09886    1.63392   18.421  < 2e-16 *** -> p-value
#  hp          -0.06823    0.01012   -6.742  1.79e-07 *** -> p-value
#  ---
#  Signif. codes:  0 | ‘***’<=0.001 | ‘**’<=0.01 | ‘*’<=0.05 | ‘.’<=0.1 | ‘ ’<=1 -> p-value
#
#Residual standard error: 3.863 on 30 degrees of freedom
#Multiple R-squared:  0.6024,	Adjusted R-squared:  0.5892 
#F-statistic: 45.46 on 1 and 30 DF,  p-value: 1.788e-07 -> ANOVA

lmFit$coefficients
#(Intercept)          hp 
#30.09886054 -0.06822828

coef(lmFit)

## predicted mpg when hp = 200
lmFit$coefficients[[1]] + lmFit$coefficients[[2]]*200

## when large data -> data frame
new_cars <- data.frame(
  hp = c(250, 320, 400, 410, 450)
)

new_cars$mpg_predict_1 <- lmFit$coefficients[[1]] + lmFit$coefficients[[2]]*new_cars[[1]]
## predict() ให้ค่าเหมือนกัน
new_cars$mpg_predict_2 <- predict(lmFit, newdata = new_cars)

## RMSE - Root mean squared error
#multiple linear regression
#mpg = f(hp, wt, am)
#mpg = intercept + b0*hp + b1*wt + b2*am
lmFit_v2 <- lm(mpg ~ hp+wt+am, data = mtcars)

coefs <- coef(lmFit_v2)
coefs[[1]] + coefs[[2]]*200 + coefs[[3]]*3.5 + coefs[[4]]*1 # y = y0 + m1x1 + m2x2 + m3x3

##Build Full Model 
lmFit_Full <- lm(mpg ~ ., data = mtcars) #ทุก column ใน mtcars เป็นตัวแปรต้นหมด
##Build Full Model 
lmFit_Full <- lm(mpg ~ . - gear, data = mtcars) #ทุก column ใน mtcars เป็นตัวแปรต้นหมดยกเว้น gear

mtcars$predicted <- predict(lmFit_Full)

sq_error <- (mtcars$mpg - mtcars$predicted)**2
rmse <- sqrt(mean(sq_error))

## Split Data
set.seed(42)
n <- nrow(mtcars)
id <- sample(1:n, size=n*0.8) #ให้ 80% ไปเป็น train data
train_data <- mtcars[id, ]
test_data <- mtcars[-id, ]

## Train Model
model1 <- lm(mpg ~ hp+wt, data = train_data)
p_train <- predict(model1)
sq_error_train <- (train_data$mpg - p_train)**2
rmse_train <- sqrt(mean(sq_error_train))

## Test Model
p_test <- predict(model1, newdata=test_data)
sq_error_test <- (test_data$mpg - p_test)**2
rmse_test <- sqrt(mean(sq_error_test))

## Print Result
paste("RMSE Train:",rmse_train) #RMSE Train: 2.26410939922013
cat("\nRMSE Test",rmse_test) #RMSE Test 3.265395

cat("RMSE Train:", rmse_train, "\nRMSE Test:",rmse_test)
#RMSE Train: 2.264109 
#RMSE Test: 3.265395

## Logistics Regression
mtcars %>% head()

str(mtcars)
glimpse(mtcars)

## convert am to factor
mtcars$am <- factor(mtcars$am, 
                    levels = c(0,1),
                    labels = c("Auto", "Manual"))
class(mtcars$am)
table(mtcars$am) #count

## Split Data
set.seed(42)
n <- nrow(mtcars)
id <- sample(1:n, size=n*0.7) #ให้ 80% ไปเป็น train data
train_data <- mtcars[id, ]
test_data <- mtcars[-id, ]

## Train Model
logis_model <- glm(am ~ mpg, data = train_data, family="binomial")
p_train <- predict(logis_model, type = "response") #output เป็น probability
train_data$pred <- if_else(p_train >= 0.5, "Manual", "Auto")
mean(train_data$am == train_data$pred)

## Test model
p_test <- predict(logis_model, newdata = test_data, type="response")
test_data$pred <- if_else(p_test >= 0.5, "Manual", "Auto")
mean(test_data$am == test_data$pred)



