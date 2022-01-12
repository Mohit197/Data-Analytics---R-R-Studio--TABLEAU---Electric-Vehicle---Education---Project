library(readxl)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(caret)
df <- read_excel("C:/Users/phatb/Desktop/data.xlsx")
head(df)
cor(df[7:13])
pairs(df[7:13],lower.panel = NULL)
### Split dataset training 70% testing 30%
set.seed(1)
dt = sort(sample(nrow(df), nrow(df)*.7))
train<-df[dt,]
test<-df[-dt,]

### model for training dataset
model <- glm(ghgScoreY~.,family = binomial, data=train[6:9])
summary(model)

lr_data_train <- data.frame(
  predictor = model$linear.predictors,
  prob = model$fitted.values,
  Type = train$VehicleType
)

ggplot(lr_data_train, aes(x = predictor, y = prob, color = Type)) + 
  geom_point()+
  scale_color_colorblind()

pr_train <- predict(model, train[6:9], type="response")
fitted.results_train <- ifelse(pr_train > 0.5,"1","0")
misClasificError_train <- mean(fitted.results_train != train$ghgScoreY)
print(paste('Accuracy for Training data set',1-misClasificError_train))

### testing dataset

pr_test <- predict(model2, test[7:9], type="response")
fitted.results_test <- ifelse(pr_test > 0.5,"1","0")
misClasificError_test <- mean(fitted.results_test != test$ghgScoreY)
print(paste('Accuracy for Testing data set',1-misClasificError_test))
