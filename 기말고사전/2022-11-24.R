# ---------------------------------------------------------------- #
# 2022-11-24 (목요일)
# 내용 : 패널티 회귀분석 
#                                                 - 김재윤 교수님 -  
# ---------------------------------------------------------------- #

# 기말고사 시험범위는 로지스틱 까지 이다.
# install.packages("glmnet")


# 데이터 셋, 데이터 나누기
library(MASS)
library(caret)
str(MASS::Boston)

set.seed(123)

train <- createDataPartition(y = Boston$medv, p = 0.7, list = FALSE)
Boston.train <- Boston[train,]
Boston.test <- Boston[-train,]

x <- model.matrix(medv ~ ., Boston.train)[,-1]
y <- Boston.train$medv

# --------------------------------------------------------------- #


# --------------------------------------------------------------- #
# 릿지 회귀 분석
library(glmnet)
set.seed(123)

Boston.cv <- cv.glmnet(x=x, y=y, family="gaussian", alpha=0)
plot(Boston.cv)

Boston.cv$lambda.min
Boston.gnet <- glmnet(x=x, y=y, family="gaussian", alpha=0, lambda = Boston.cv$lambda.min)

Boston.test.x <- model.matrix(medv ~ ., Boston.test)[,-1]
Boston.pred <- predict(Boston.gnet, newx=Boston.test.x)
# 4566465

postResample(pred=Boston.pred, obs=Boston.test$medv)
#     RMSE Rsquared      MAE 
# 5.270168 0.707766 3.343058 

# --------------------------------------------------------------- #
# 라쏘 회귀 분석
set.seed(123)
Boston.cv <- cv.glmnet(x=x, y=y, family="gaussian", alpha=1)

Boston.cv$lambda.min

plot(Boston.cv)

Boston.cv$lambda.1se

coef(Boston.cv, Boston.cv$lambda.min)

# lambda.min 사용한 경우
Boston.gnet1 <- glmnet(x=x, y=y, family="gaussian", alpha=1, lambda = Boston.cv$lambda.min)
Boston.pred1_t <- predict(Boston.gnet1, newx=x) #학습데이터
postResample(pred=Boston.pred1_t, obs=y)
# RMSE  Rsquared       MAE 
# 4.5205597 0.7480793 3.2266755 

Boston.pred1 <- predict(Boston.gnet1, newx=Boston.test.x) #테스트데이터
postResample(pred=Boston.pred1, obs=Boston.test$medv)
# RMSE  Rsquared       MAE 
# 5.1150340 0.7197637 3.3091787 

# lambda.1se 사용한 경우
Boston.gnet2 <- glmnet(x=x, y=y, family="gaussian", alpha=1, lambda = Boston.cv$lambda.1se)
Boston.pred2_t <- predict(Boston.gnet2, newx=x) #학습데이터
postResample(pred=Boston.pred2_t, obs=y)
# RMSE  Rsquared       MAE 
# 4.8034175 0.7192424 3.3642974 

Boston.pred2 <- predict(Boston.gnet2, newx=Boston.test.x) #테스트데이터
postResample(pred=Boston.pred2, obs=Boston.test$medv) 
# RMSE  Rsquared       MAE 
# 5.5656605 0.6795993 3.6151777

# --------------------------------------------------------------- #
# Elastic Net 사용
library(caret)
set.seed(123)
Boston.cv <- train(form = medv ~ ., data=Boston.train, method ="glmnet",
    trControl = trainControl(method = "cv", number = 10), tuneLength=10)
Boston.cv$bestTune #최적의 알파와 람다 값
Boston.gnet_elastic <- glmnet(x=x, y=y, family="gaussian",
  alpha=Boston.cv$bestTune$alpha,
    lambda = Boston.cv$bestTune$lambda)
coef(Boston.gnet_elastic)
Boston.pred_elastic_t <- predict(Boston.gnet_elastic, newx=x) #학습데이터
postResample(pred=Boston.pred_elastic_t, obs=y)
#     RMSE Rsquared      MAE 
# 4.533393 0.746910 3.216357 

Boston.pred_dlastic_test <- predict(Boston.gnet_elastic, newx=Boston.test.x) #테스트데이터
postResample(pred=Boston.pred_dlastic_test, obs=Boston.test$medv) 
#      RMSE  Rsquared       MAE 
# 5.1766444 0.7148912 3.3115941








