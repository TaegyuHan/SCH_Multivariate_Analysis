# ------------------------------------------------------------------ #
# 데이터 분석 진행
# ------------------------------------------------------------------ #
# install.packages("ISLR")
# install.packages("mltools")
library(mltools)
library(ISLR)
library(data.table)
library(caret)
library(ROCR)
library(e1071)
data(Default)


str(Default)
# 'data.frame':	10000 obs. of  4 variables:
# $ default: Factor w/ 2 levels "No","Yes": 1 1 1 1 1 1 1 1 1 1 ...
# $ student: Factor w/ 2 levels "No","Yes": 1 2 1 1 1 2 1 2 1 1 ...
# $ balance: num  730 817 1074 529 786 ...
# $ income : num  44362 12106 31767 35704 38463 ...

head(Default)

data <- Default

# default : 채무 불이행 
# student : 학생여부
# balance : 채무잔액
# income : 연수수입

set.seed(123456798)

n <- nrow(Default)
idx <- 1:n
training_idx <- sample(idx, n * .60)
idx <- setdiff(idx, training_idx)
validate_idx <- sample(idx, n * .20)
test_idx <- setdiff(idx, validate_idx)
training <- data[training_idx,]
validation <- data[validate_idx,]
test <- data[test_idx,]

data_logit_full <- glm(default ~ ., data = training, family = "binomial") 
summary(data_logit_full)

# Call:
#   glm(formula = default ~ ., family = "binomial", data = training)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -2.4918  -0.1317  -0.0502  -0.0177   3.8222  
# 
# Coefficients:
#                 Estimate Std. Error z value Pr(>|z|)    
#   (Intercept) -1.159e+01  6.794e-01 -17.060   <2e-16 ***
#   studentYes  -3.053e-01  3.198e-01  -0.955    0.340    
#   balance      5.939e-03  3.146e-04  18.880   <2e-16 ***
#   income       8.936e-06  1.088e-05   0.822    0.411    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 1719.93  on 5999  degrees of freedom
# Residual deviance:  900.33  on 5996  degrees of freedom
# AIC: 908.33
# 
# Number of Fisher Scoring iterations: 8

y_obs_t <- training$default

pre_logit_t <- predict(data_logit_full, newdata = training, type = "response")
pre_logit_t <- factor(ifelse(pre_logit_t > 0.5, 1, 0))
y_obs_t <- factor(ifelse(y_obs_t == "No", 1, 0))

ROCR::prediction(pre_logit_t, y_obs_t)

pred_logit_t <- prediction()

caret::confusionMatrix(data=factor(pre_logit_t), reference=factor(y_obs_t))
# Confusion Matrix and Statistics
# 
# Reference
# Prediction   No  Yes
# No  5777  159
# Yes   14   50
# 
# Accuracy : 0.9712          
# 95% CI : (0.9666, 0.9753)
# No Information Rate : 0.9652          
# P-Value [Acc > NIR] : 0.005206        
# 
# Kappa : 0.3558          
# 
# Mcnemar's Test P-Value : < 2.2e-16       
#                                           
#             Sensitivity : 0.9976          
#             Specificity : 0.2392          
#          Pos Pred Value : 0.9732          
#          Neg Pred Value : 0.7813          
#              Prevalence : 0.9652          
#          Detection Rate : 0.9628          
#    Detection Prevalence : 0.9893          
#       Balanced Accuracy : 0.6184          
#                                           
#        'Positive' Class : No    

performance(pred_logit_t,"auc")@y.values[[1]]

