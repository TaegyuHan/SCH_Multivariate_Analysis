# ---------------------------------------------------------------- #
# 2022-11-22 (수요일)
# 내용 : 
# 과제 제출 꼭 제출해주세요!
#                                                 - 김재윤 교수님 -  
# ---------------------------------------------------------------- #

# 과제 이번주 내로 작성해서 올려주세요!
# 프로젝트 학생들 발표자료를 올려주신다. 하신다.
# 지각을 해도 제출해주기 바란다.

# 60% 20% 20% 결국에는 여러번을 샘플해야 한다. Train에 해당하는 평균값을 계산을 해서
# 비교 해볼필요성은 있다.



# UCLA 대학원 입학 데이터
data <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")

str(data)

data$rank <- factor(data$rank)
data$admit <- factor(data$admit)


# 데이터 나누기 60%, 검증 20% 테스트 20%
set.seed(123456789)
n <- nrow(data)
idx <- 1:n
training_idx <- sample(idx, n * .60)
idx <- setdiff(idx, training_idx)
validate_idx <- sample(idx, n * .20)
test_idx <- setdiff(idx, validate_idx)
training <- data[training_idx,]
validation <- data[validate_idx,]
test <- data[test_idx,]


# 로지스틱 회귀 분석
data_logit_full <- glm(admit ~.,
                       data = training,
                       family = "binomial")

summary(data_logit_full)
# Call:
#   glm(formula = admit ~ ., family = "binomial", data = training)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -1.4359  -0.8695  -0.6024   1.1159   2.0794  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)   
# (Intercept) -2.865108   1.458559  -1.964  0.04949 * 
#   gre          0.001438   0.001348   1.066  0.28621   
#   gpa          0.580534   0.413605   1.404  0.16044   
#   rank2       -0.570128   0.410235  -1.390  0.16460   
#   rank3       -1.299010   0.457234  -2.841  0.00450 **
#   rank4       -1.624711   0.568015  -2.860  0.00423 **
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 291.50  on 239  degrees of freedom
# Residual deviance: 269.74  on 234  degrees of freedom
# AIC: 281.74
# 
# Number of Fisher Scoring iterations: 4


# 로지스틱 회귀분석으로 적합값/예측값 확인 (학습데이터의 경우)
# install.packages("caret")
# install.packages("ROCR")
# install.packages("e1071")

library(caret)
library(ROCR)
library(e1071)

y_obs_t <- training$admit # 학습 데이터의 실제 값

pre_logit_t <- predict(data_logit_full,
                      newdata = training,
                      type = "response") # 적합
pred_logit_t <- prediction(pre_logit_t, y_obs_t) # 적합값, 실제값


confusionMatrix(factor(ifelse(pre_logit_t > 0.5, 1.0, 0.0)), y_obs_t)  # 혼돈 행렬
performance(pred_logit_t, "auc")@y.values[[1]]  # AUC 값
# [1] 0.6848071


# 로지스틱 회귀분석으로 적합값/예측값 확인(검증데이터 경우)

y_obs_v <- validation$admit

pre_logit_v <- predict(data_logit_full,
                       newdata = validation,
                       type = "response") # 적합
pred_logit_v <- prediction(pre_logit_v, y_obs_v) # 적합값, 실제값

confusionMatrix(factor(ifelse(pre_logit_v > 0.5, 1, 0)), y_obs_v)  # 혼돈 행렬
performance(pred_logit_v, "auc")@y.values[[1]]  # AUC 값
# [1] 0.6531593

y_obs_tt <- test$admit

pre_logit_tt <- predict(data_logit_full,
                       newdata = test,
                       type = "response") # 적합
pred_logit_tt <- prediction(pre_logit_tt, y_obs_tt) # 적합값, 실제값

confusionMatrix(factor(ifelse(pre_logit_tt > 0.5, 1, 0)), y_obs_tt)  # 혼돈 행렬
performance(pred_logit_v, "auc")@y.values[[1]]  # AUC 값
# [1] 0.6531593

# 나무모형으로 적합값/예측값 확인 (패키지 rpart필요, 학습데이터의 경우)
library(rpart)
data_tr <- rpart(admit ~ ., data = training)
opar <- par(mfrow = c(1,1), xpd = NA)
plot(data_tr)
text(data_tr, use.n = TRUE)

pre_tr_t <- predict(data_tr, newdata = training) # 학습데이터 적합
pred_tr_t <- prediction(pre_tr_t[,"1"], y_obs_t)
confusionMatrix(factor(ifelse(pre_tr_t[,"1"] > 0.5, 1, 0)), y_obs_t)
performance(pred_tr_t, "auc")@y.values[[1]]
# [1] 0.6774315

# 나무 모형으로 적합값/예측값 확인
pre_tr_v <- predict(data_tr, newdata = validation)
pred_tr_v <- prediction(pre_tr_v[,"1"], y_obs_v)
confusionMatrix(factor(ifelse(pre_tr_v[,"1"] > 0.5, 1, 0)), y_obs_v)
performance(pred_tr_v, "auc")@y.values[[1]]
# [1] 0.6298077

# 랜덤 포레스트로 적합값/예측값 확인
# install.packages("randomForest")
library(randomForest)

data_rf <- randomForest(admit ~ ., data=training)
varImpPlot(data_rf)
pre_rf_t <- predict(data_rf, newdata = training, type='prob')[,'1']
pred_rf_t<- prediction(pre_rf_t, y_obs_t)

confusionMatrix(factor(ifelse(pre_rf_t>0.5,1,0)), y_obs_t)
performance(pred_rf_t, "auc")@y.values[[1]]
# [1] 0.9814568

# 랜덤 포레스트로 적합값/예측값 확인
pre_rf_v <- predict(data_rf, newdata = validation, type="prob")[,'1']
pred_rf_v <- prediction(pre_rf_v, y_obs_v)
confusionMatrix(factor(ifelse(pre_rf_v>0.5,1,0)), y_obs_v)
performance(pred_rf_v, "auc")@y.values[[1]]
# [1] 0.5591518

# 부스팅 으로 적합값/예측값 확인
# install.packages("gbm")
library(gbm)

training$admit <- as.numeric(ifelse(training$admit=='0',0,1))
data_gbm <- gbm(admit ~ ., data=training, distribution = "bernoulli", n.trees = 500, 
                cv.folds=5, verbose=TRUE)
best_iter <- gbm.perf(data_gbm, method="cv")
pre_gbm_t <- predict(data_gbm, n.trees=best_iter, newdata = training, type = 
                       'response')
pred_gbm_t <- prediction(pre_gbm_t, y_obs_t)
confusionMatrix(factor(ifelse(pre_gbm_t>0.5,1,0)), y_obs_t)
performance(pred_gbm_t, "auc")@y.values[[1]]
# [1] 0.7123087


# 부스팅으로 적합값/예측값 확인
pre_gbm_v <- predict(data_gbm, n.trees=best_iter, newdata = validation, type = 
                       'response')
pred_gbm_v <- prediction(pre_gbm_v, y_obs_v)
confusionMatrix(factor(ifelse(pre_gbm_v>0.5,1,0)), y_obs_v)
performance(pred_gbm_v, "auc")@y.values[[1]]
# [1] 0.5844494

# 테이블 형태의 결과 필요
# 각 알고리즘 별로 성과 (정확도, AUC) 비교 [학습, 검증, 테스트]
#             Logit, DT, RF, GBM
# Train (AUC) 0.725(0.684)  0.7792(0.9780) 0.8417 0.7375(0.7093)
# Valid (AUC) 0.6625(0.653) 0.6375(0.6692) 0.675  0.6625(0.6177)
# Test  (AUC) 0.725(0.733)  0.7125(0.7221) 0.725  0.7125(0.7695)

# 각 알고리즘별로 반복 측정된 성과 (평균정확도와 표준편차) 비교 => 100번 수행
#             Logit, DT, RF, GBM
# Train (SD) 0.684 0.747  0.9780 0.7093
# Valid (SD) 0.653 0.5786 0.6692 0.6177
# Test  (SD) 0.733 0.7534 0.7221 0.4251










