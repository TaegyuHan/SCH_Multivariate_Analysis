# install.packages("ISLR")
library(ISLR)
data(Default)
data <-Default

# 범주형 데이터에 대한 수치화
data$default <- factor(ifelse(data$default=='Yes',1,0))
data$student <- factor(ifelse(data$student=='Yes',1,0))
# 1.로지스틱 회귀모형 적합

glm_ISLR <- glm(data$default~., data=data, family='binomial')
summary(glm(data$default~., data=data, family='binomial'))

str(data[-4])

glm_ISLR2 <-glm(default~., data=data[-4], family = 'binomial')
summary(glm_ISLR2)

# 2. 혼돈행렬
library(ROCR)
pred_glm <- predict(glm_ISLR2, newdata= data, type = 'response')
predict_glm <- prediction(pred_glm, data$default)
library(caret)

confusionMatrix(factor(ifelse(pred_glm>0.5,1,0)), data$default)
performance(predict_glm, "auc")@y.values[[1]]

data$diff <- data$income - data$balance

str(data)


