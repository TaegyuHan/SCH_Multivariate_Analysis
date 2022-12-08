library(mltools)
library(ISLR)
library(data.table)
library(caret)
library(ROCR)
library(e1071)
library(PerformanceAnalytics)
library(car)
library(MASS)


str(attitude)
# 'data.frame':	30 obs. of  7 variables:
# $ rating    : num  43 63 71 61 81 43 58 71 72 67 ...
# $ complaints: num  51 64 70 63 78 55 67 75 82 61 ...
# $ privileges: num  30 51 68 45 56 49 42 50 72 45 ...
# $ learning  : num  39 54 69 47 66 44 56 55 67 47 ...
# $ raises    : num  61 63 76 54 71 54 66 70 71 62 ...
# $ critical  : num  92 73 86 84 83 49 68 66 83 80 ...
# $ advance   : num  45 47 48 35 47 34 35 41 31 41 ...

# 종속 변수 rating
library(PerformanceAnalytics)

cor(attitude)
PerformanceAnalytics::chart.Correlation(attitude, histogram = TRUE, pch = 15)

car::vif(lm(rating~., data = attitude))
# complaints privileges   learning     raises   critical 
# 2.667060   1.600891   2.271043   3.078226   1.228109 
# advance 
# 1.951591 

summary(lm(rating~., data = attitude))
# Call:
#   lm(formula = rating ~ ., data = attitude)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -10.9418  -4.3555   0.3158   5.5425  11.5990 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 10.78708   11.58926   0.931 0.361634    
# complaints   0.61319    0.16098   3.809 0.000903 ***
# privileges  -0.07305    0.13572  -0.538 0.595594    
# learning     0.32033    0.16852   1.901 0.069925 .  
# raises       0.08173    0.22148   0.369 0.715480    
# critical     0.03838    0.14700   0.261 0.796334    
# advance     -0.21706    0.17821  -1.218 0.235577    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 7.068 on 23 degrees of freedom
# Multiple R-squared:  0.7326,	Adjusted R-squared:  0.6628 
# F-statistic:  10.5 on 6 and 23 DF,  p-value: 1.24e-05


summary(lm(rating~., data = attitude[c(1, 2)]))
# Call:
#   lm(formula = rating ~ ., data = attitude[c(1, 2)])
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -12.8799  -5.9905   0.1783   6.2978   9.6294 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 14.37632    6.61999   2.172   0.0385 *  
#   complaints   0.75461    0.09753   7.737 1.99e-08 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 6.993 on 28 degrees of freedom
# Multiple R-squared:  0.6813,	Adjusted R-squared:  0.6699 
# F-statistic: 59.86 on 1 and 28 DF,  p-value: 1.988e-08


set.seed(1606)
n <- nrow(attitude[c(1, 2)])
idx <- 1:n
training_idx <- sample(idx, n * .60)
idx <- setdiff(idx, training_idx)
validate_idx <- sample(idx, n * .20)
test_idx <- setdiff(idx, validate_idx)
training <- attitude[c(1, 2)][training_idx,]
validation <- attitude[c(1, 2)][validate_idx,]
test <- attitude[c(1, 2)][test_idx,]

data_lm_full <- lm(rating ~ ., data=training)

hist(data_lm_full$residuals)
plot(data_lm_full, which = 1)
plot(data_lm_full, which = 2)

# ------------------------------------------------- #
library(ISLR)
data("Default")

str(Default)
# 'data.frame':	10000 obs. of  4 variables:
# $ default: Factor w/ 2 levels "No","Yes": 1 1 1 1 1 1 1 1 1 1 ...
# $ student: Factor w/ 2 levels "No","Yes": 1 2 1 1 1 2 1 2 1 1 ...
# $ balance: num  730 817 1074 529 786 ...
# $ income : num  44362 12106 31767 35704 38463 ...

n <- nrow(Default)
idx <- 1:n
training_idx <- sample(idx, n * .60)
idx <- setdiff(idx, training_idx)
validate_idx <- sample(idx, n * .20)
test_idx <- setdiff(idx, validate_idx)
training <- Default[training_idx,]
validation <- Default[validate_idx,]
test <- Default[test_idx,]

summary(glm(default ~ ., data = training, family = "binomial"))

y_obs_t <- training$default

pre_logit_t <- predict(data_logit_full, newdata = training, type = "response")
pre_logit_t <- factor(ifelse(pre_logit_t > 0.5, 1, 0))
y_obs_t <- factor(ifelse(y_obs_t == "No", 1, 0))

pred_logit_t <- prediction(pre_logit_t, y_obs_t)

caret::confusionMatrix(data=factor(pre_logit_t), reference=factor(y_obs_t))

