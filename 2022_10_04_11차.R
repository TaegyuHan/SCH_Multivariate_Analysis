# ---------------------------------------------------------------- #
# 2022-10-04 (목요일) 6주 11차 다변량 분석 강의
# 내용 : 회귀 분석
#                                                 - 김재윤 교수님 -  
# ---------------------------------------------------------------- #


# library
# install.packages("psych")
# install.packages("Hmisc")
library(psych)
library(Hmisc)
library(tidyverse)


# ---------------------------------------------------------------- #
# 1. 회귀함수의 선형성
#     - 선형회귀 함수가 적합한지 적합사지 않은지에 대한 문제는 자료의
#         산점도로 확인
#     - 즉, 잔차를 각 설명변수(X) 또는 반응변수(Y)와의 산점도를 통해 확인

# 2. 오차항의 등분산성과 독립성
#     - 보통 적합된 값(fiited values)과 잔차와의 산점도를 통해 0을 중심으로 
#       랜덤한 형태를 만족해야함
#     - 잔차에 대한 돌깁성 검정은 Durbin-Watson 검정통계량을 통해 확인

# 3. 오차항의 정규성
#     - 잔차 히스토그램 확인
#     - 잔차들에 대해 정규 Q-Q 그림을 통해 확인
#     - 정규성을 만족한다는 귀무가설에 대한 Shapiro-Wilks 검정통계량을 통해
#        확인

# ------------------------------------ # 
# 실습 1
x <- c( 1.9, 0.8, 1.1, 0.1, -0.1, 4.4, 4.6, 1.6, 5.5, 3.4)
y <- c( 0.7, -1.0, -0.2, -1.2, -0.1, 3.4, 0.0, 0.8, 3.7, 2.0)
plot(x, y)

cor(x, y)
# [1] 0.80224

out <- lm(y ~ x)
# Call:
#   lm(formula = y ~ x)
# 
# Coefficients:
#   (Intercept)            x  
# -0.7861       0.6850  

summary(out)
# Call:
#   lm(formula = y ~ x)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.3651 -0.4036  0.3208  0.6613  1.1720 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)   
# (Intercept)  (-0.7861)     0.5418  -1.451  0.18485   
# x             (0.6850)     0.1802   3.801  0.00523 **
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1.083 on 8 degrees of freedom
# Multiple R-squared:  0.6436,	Adjusted R-squared:  0.599 
# F-statistic: 14.45 on 1 and 8 DF,  p-value: 0.005231

# 회귀 모형 적합식
# y = 0.6850x - 0.7861

# 그림으로 표현
plot(x, y)
abline(out)

# 적합된 데이터 및 예측하기
pred_y <- predict(out, newdata = data.frame(x=x))
out$fitted.values
# 1           2           3           4           5 
# 0.51543194 -0.23811424 -0.03260165 -0.71764364 -0.85465203 
# 6           7           8           9          10 
# 2.22803692  2.36504531  0.30991935  2.98158310  1.54299493 

# 새로운 데이터로 예측
pred_1 <- predict(out, newdata = data.frame(x = 2.3))
pred_2 <- predict(out, newdata = data.frame(x = c(1, 2.2, 6.7)))
# RMSE를 계산할 필요가 있다. > 성능확인을 위해서

# ------------------------------------ # 
# 실습 2
volume <- c(412, 953, 929, 1492, 419, 1010, 595, 1034)
weight <- c(250, 700, 650, 975, 350, 950, 425, 725)

book.lm <- lm(weight ~ volume)
# Call:
#   lm(formula = weight ~ volume)
# 
# Coefficients:
#   (Intercept)       volume  
# 41.3725       0.6859  

summary(book.lm)
# Call:
#   lm(formula = weight ~ volume)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -89.674 -39.888 -25.005   9.066 215.910 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  41.3725    97.5588   0.424 0.686293    
# volume        0.6859     0.1059   6.475 0.000644 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 102.2 on 6 degrees of freedom
# Multiple R-squared:  0.8748,	Adjusted R-squared:  0.8539 
# F-statistic: 41.92 on 1 and 6 DF,  p-value: 0.0006445

anova(book.lm)
# Analysis of Variance Table
# 
# Response: weight
# Df Sum Sq Mean Sq F value    Pr(>F)    
# volume     1 437878  437878  41.923 0.0006445 ***
#   Residuals  6  62669   10445                      
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# 시각화
plot(volume, weight)
lines(volume, book.lm$fitted.values)

# 잔차분석 그래프
plot(book.lm, which = 1)

plot(book.lm, which = 2, pch = 16)


# ------------------------------------ # 
# D.I.Y (다중 회귀 분석)
# 과제

# -------------- #
# number1

data(women)
str(women)
# 'data.frame':	15 obs. of  2 variables:
# $ height: num  58 59 60 61 62 63 64 65 66 67 ...
# $ weight: num  115 117 120 123 126 129 132 135 139 142 ...

women %>% is.na() %>% sum()
# [1] 0

# 산술 통계
summary(women$weight)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 115.0   124.5   135.0   136.7   148.0   164.0 
summary(women$height)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 58.0    61.5    65.0    65.0    68.5    72.0 

# 상관분석
cor(women$weight, women$height)
# [1] 0.9954948

women.lm <- lm(women$weight ~ women$height)
# Call:
#   lm(formula = women$weight ~ women$height)
# 
# Coefficients:
#   (Intercept)  women$height  
# -87.52          3.45  

summary(women.lm)
# Call:
#   lm(formula = women$weight ~ women$height)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -1.7333 -1.1333 -0.3833  0.7417  3.1167 
# 
# Coefficients: (회귀계수의 유의성 검정)
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)    (-87.51667)    5.93694  -14.74 1.71e-09 ***
#   women$height   (3.45000)    0.09114   37.85 1.09e-14 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1.525 on 13 degrees of freedom
# Multiple R-squared:  0.991,	Adjusted R-squared:  0.9903 

# (회귀모형에 대한 적합성 검정)
# F-statistic:  1433 on 1 and 13 DF,  p-value: 1.091e-14


# 1. weight에 대한 height의 산점도를 그리시오.
plot(women$weight, women$height)

# 2. 단순선형회귀직선을 구하여 산점도에 함께 나타내시오.
lines(women.lm$fitted.values, women$height)

# 3. 모형 적합성에 대해 검정하시오.
# 귀무가설 : 모형은 적합하지 않다.
# 대립가설 : 모형은 적합하다.
# 결론 : 유의 확률(p-value) : 1.091e-14이므로 유의수준 0.05에서 회귀모형은 
# 적합하다. 즉, 독립변수가 족송변수에 영향을 준다.


# 회귀계수가 유의한지 검정하시오.
# 귀무가설 : 독립변수는 종속변수에게 영향을 주지 않는다.
# 대립가설 : 독립변수는 종속변수에게 영향을 준다.
# 결론 : 유의 확률(p-value) : 1.09e-14 이므로 유의수준 0.05에서 독립변수는 종속
# 변수에게 통계적으로 유의한 영향을 주는 것으로 나타났다.
# 따라서, height는 weight에 유의한 영향을 주는 것으로 나타났다.

# 결정계수를 구하고 해석하시오.
# y = -87.52x + 3.45

# 잔차에 대해 독립성, 등분산성, 정규성을 만족하는 잔차그림
# Q - Q 그림을 그리고 설명 하시오.

# 독립성
plot(women.lm, which = 1)

# 등분산성
plot(women.lm, which = 3)

# 정규성
plot(women.lm, which = 2)

# (잔차)히스토그램
hist(women.lm$residuals)


# -------------- #
# number2

data(airquality)
airquality %>% is.na() %>% sum()
# [1] 44

airquality <- airquality[,1:4] %>% na.omit()
airquality %>% is.na() %>% sum()
# [1] 0

str(airquality[,1:4])
# 'data.frame':	111 obs. of  4 variables:
# $ Ozone  : int  41 36 12 18 23 19 8 16 11 14 ...
# $ Solar.R: int  190 118 149 313 299 99 19 256 290 274 ...
# $ Wind   : num  7.4 8 12.6 11.5 8.6 13.8 20.1 9.7 9.2 10.9 ...
# $ Temp   : int  67 72 74 62 65 59 61 69 66 68 ...




# Ozone 종속변수이며, Solar.R(일조량), Wind(풍속), Temp(기온)
# (식)> Ozone ~ Solar.R + Wind + Temp

airquality.lm <- lm(airquality$Ozone 
                    ~ airquality$Solar.R 
                    + airquality$Wind 
                    + airquality$Temp)
# Call:
#   lm(formula = airquality$Ozone ~ airquality$Solar.R + airquality$Wind + 
#        airquality$Temp)
# 
# Coefficients:
#   (Intercept)  airquality$Solar.R     airquality$Wind     airquality$Temp  
# -64.34208             0.05982            -3.33359             1.65209  


# 1. 각 변수들간의 상관분석 및 산점도 그리기
psych::pairs.panels(airquality[,1:4])


# 2. 3개의 독립변수를 활용하여 다중회귀분석을 실시
summary(airquality.lm)
# Call:
#   lm(formula = airquality$Ozone ~ airquality$Solar.R + airquality$Wind + 
#        airquality$Temp)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -40.485 -14.219  -3.551  10.097  95.619 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)        -64.34208   23.05472  -2.791  0.00623 ** 
#   airquality$Solar.R   0.05982    0.02319   2.580  0.01124 *  
#   airquality$Wind     -3.33359    0.65441  -5.094 1.52e-06 ***
#   airquality$Temp      1.65209    0.25353   6.516 2.42e-09 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 21.18 on 107 degrees of freedom
# Multiple R-squared:  0.6059,	Adjusted R-squared:  0.5948 
# F-statistic: 54.83 on 3 and 107 DF,  p-value: < 2.2e-16

anova(airquality.lm)
# Analysis of Variance Table
# 
# Response: airquality$Ozone
# Df Sum Sq Mean Sq F value    Pr(>F)    
# airquality$Solar.R   1  14780   14780  32.944 8.946e-08 ***
#   airquality$Wind      1  39969   39969  89.094 9.509e-16 ***
#   airquality$Temp      1  19050   19050  42.463 2.424e-09 ***
#   Residuals          107  48003     449                      
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


# 3. 모형 적합성 및 회귀계수의 유의성 검정하기
# 회귀모형의 유의성 검정
# 귀무가설 : 회귀모형은 타당하지 않다.
# 대립가설 : 회귀모형은 타당하다.
# 결론 : 유의 확률이 2.2e-16 유의수준 0.05에서 회귀모형은 타당하다.

# 회귀계수의 유의성 검정
# 귀무가설 : 독립변수는 종속변수에게 영향을 주지 않는다.
# 대립가설 : 독립변수는 종속변수에게 영향을 준다.

# 해석
# Solar : 유의확률 = 8.946e-08 => 대립가설
# Wind : 유의확률 = 9.509e-16 => 대립가설
# Temp : 유의확률 = 2.424e-09 => 대립가설
# 결론 모든 독립변수는 종속변수에 영향을 준다.

# - 잔차분석(그래프 그리기: 
#         - (잔차)히스토그램, 
#         - 적합값과 잔차 산점도, 
#         - Q-Q plot)
)

# 잔차 히스토그램
hist(airquality.lm$residuals)

# 적합값과 잔차 산점도
plot(airquality.lm, which = 3)

# Q-Q plot
plot(airquality.lm, which = 2)


# -------------- #
# number3
library(MASS)
boston.data <- MASS::Boston
str(boston.data)
# 'data.frame':	506 obs. of  14 variables:
# $ crim   : num  0.00632 0.02731 0.02729 0.03237 0.06905 ...
# $ zn     : num  18 0 0 0 0 0 12.5 12.5 12.5 12.5 ...
# $ indus  : num  2.31 7.07 7.07 2.18 2.18 2.18 7.87 7.87 7.87 7.87 ...
# $ chas   : int  0 0 0 0 0 0 0 0 0 0 ...
# $ nox    : num  0.538 0.469 0.469 0.458 0.458 0.458 0.524 0.524 0.524 0.524 ...
# $ rm     : num  6.58 6.42 7.18 7 7.15 ...
# $ age    : num  65.2 78.9 61.1 45.8 54.2 58.7 66.6 96.1 100 85.9 ...
# $ dis    : num  4.09 4.97 4.97 6.06 6.06 ...
# $ rad    : int  1 2 2 3 3 3 5 5 5 5 ...
# $ tax    : num  296 242 242 222 222 222 311 311 311 311 ...
# $ ptratio: num  15.3 17.8 17.8 18.7 18.7 18.7 15.2 15.2 15.2 15.2 ...
# $ black  : num  397 397 393 395 397 ...
# $ lstat  : num  4.98 9.14 4.03 2.94 5.33 ...
# $ medv   : num  24 21.6 34.7 33.4 36.2 28.7 22.9 27.1 16.5 18.9 ...

#factor형으로 변환
boston.data$chas <- factor(boston.data$chas)


#결측치확인
boston.data %>% is.na() %>% sum()
# [1] 0


#기초통계량
summary(boston.data)
# crim                zn             indus            chas              nox        
# Min.   : 0.00632   Min.   :  0.00   Min.   : 0.46   Min.   :0.00000   Min.   :0.3850  
# 1st Qu.: 0.08205   1st Qu.:  0.00   1st Qu.: 5.19   1st Qu.:0.00000   1st Qu.:0.4490  
# Median : 0.25651   Median :  0.00   Median : 9.69   Median :0.00000   Median :0.5380  
# Mean   : 3.61352   Mean   : 11.36   Mean   :11.14   Mean   :0.06917   Mean   :0.5547  
# 3rd Qu.: 3.67708   3rd Qu.: 12.50   3rd Qu.:18.10   3rd Qu.:0.00000   3rd Qu.:0.6240  
# Max.   :88.97620   Max.   :100.00   Max.   :27.74   Max.   :1.00000   Max.   :0.8710  
# rm             age              dis              rad              tax       
# Min.   :3.561   Min.   :  2.90   Min.   : 1.130   Min.   : 1.000   Min.   :187.0  
# 1st Qu.:5.886   1st Qu.: 45.02   1st Qu.: 2.100   1st Qu.: 4.000   1st Qu.:279.0  
# Median :6.208   Median : 77.50   Median : 3.207   Median : 5.000   Median :330.0  
# Mean   :6.285   Mean   : 68.57   Mean   : 3.795   Mean   : 9.549   Mean   :408.2  
# 3rd Qu.:6.623   3rd Qu.: 94.08   3rd Qu.: 5.188   3rd Qu.:24.000   3rd Qu.:666.0  
# Max.   :8.780   Max.   :100.00   Max.   :12.127   Max.   :24.000   Max.   :711.0  
# ptratio          black            lstat            medv      
# Min.   :12.60   Min.   :  0.32   Min.   : 1.73   Min.   : 5.00  
# 1st Qu.:17.40   1st Qu.:375.38   1st Qu.: 6.95   1st Qu.:17.02  
# Median :19.05   Median :391.44   Median :11.36   Median :21.20  
# Mean   :18.46   Mean   :356.67   Mean   :12.65   Mean   :22.53  
# 3rd Qu.:20.20   3rd Qu.:396.23   3rd Qu.:16.95   3rd Qu.:25.00  
# Max.   :22.00   Max.   :396.90   Max.   :37.97   Max.   :50.00  



#상관분석
# install.packages("corrplot")
library(corrplot)

# 데이터 나누기
set.seed(1606)
n <- nrow(boston.data)
idx <- 1:n

train_idx <- sample(idx, size = n * 0.65) # 70% training data
idx <- setdiff(idx, train_idx) #train_data로 선택된 값을 제외한 idx만 추출

validate_idx <- sample(idx, size = n * 0.2) #나머지 중 100% valid data
test_idx <- setdiff(idx, validate_idx) #나머지 데이터 test data

training <- boston.data[train_idx,]
validation <- boston.data[validate_idx,]
test <- boston.data[test_idx,]

#확인
training %>% nrow()
# [1] 328
validation %>% nrow()
# [1] 101
test %>% nrow()
# [1] 77

# 수치형 변수만
boston.data[,-4] %>% 
  cor() %>% 
  corrplot(method = "number")

# 학습데이터로 모형 적합
boston.data.lm <- lm(medv~., training)
summary(boston.data.lm)
# Call:
#   lm(formula = medv ~ ., data = training)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -10.7779  -2.7917  -0.6996   1.8940  25.6443 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  30.597370   6.472844   4.727 3.56e-06 ***
#   crim         -0.105146   0.035900  -2.929 0.003673 ** 
#   zn            0.043363   0.017482   2.480 0.013691 *  
#   indus        -0.055383   0.076866  -0.721 0.471792    
# chas1         2.880243   1.083849   2.657 0.008312 ** 
#   nox         -17.412105   4.769490  -3.651 0.000310 ***
#   rm            4.524753   0.551838   8.199 7.97e-15 ***
#   age          -0.010986   0.017129  -0.641 0.521796    
# dis          -1.607727   0.253495  -6.342 8.70e-10 ***
#   rad           0.304656   0.084490   3.606 0.000366 ***
#   tax          -0.012165   0.004915  -2.475 0.013892 *  
#   ptratio      -0.801398   0.163204  -4.910 1.52e-06 ***
#   black         0.007910   0.003465   2.283 0.023148 *  
#   lstat        -0.456135   0.068098  -6.698 1.10e-10 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 4.632 on 289 degrees of freedom
# Multiple R-squared:  0.7549,	Adjusted R-squared:  0.7439 
# F-statistic: 68.47 on 13 and 289 DF,  p-value: < 2.2e-16


#잔차분석
boston.data.lm$residuals

# 독립성
plot(boston.data.lm,which = 1)

# (잔차)히스토그램
hist(boston.data.lm$residuals)

# 정규성
plot(boston.data.lm, which = 2)

#RMSE값 구하기
# install.packages("MLmetrics")
library(MLmetrics)

RMSE(boston.data.lm$fitted.values,training$medv)
# [1] 5.451836

p <- predict(boston.data.lm, training)


#검증 데이터로 예측하기
pred <- predict(boston.data.lm, newdata = validation)
RMSE(pred, validation$medv)

#2차항을 포함한 회귀
boston.data.lm.re <- lm(medv ~ .+.^2, training)
pred.lm.re <- predict(boston.data.lm.re, validation)

#decision tree
# install.packages("rpart")
library(rpart)

DT <- rpart(medv ~ ., data = training)
pred_dt <- predict(DT, newdata = validation)

#random forest
# install.packages("randomForest")
library(randomForest)
RF <- randomForest(medv ~ ., data = training)
pred_rf <- predict(RF, newdata = validation)

#boosting
# install.packages('gbm')
library(gbm)
GBM <- gbm(medv ~ ., data = training)
pred_b <- predict(GBM, newdata = validation)


#정확도(RMSE) 평가
RMSE(boston.data.lm$fitted.values,
     training$medv) #training data

#RMSE값 비교
RMSE(pred.lm.re, validation$medv)
# [1] 3.676451

RMSE(pred_dt, validation$medv)
# [1] 3.832521

RMSE(pred_rf, validation$medv)
# [1] 2.854533

RMSE(pred_b, validation$medv)
# [1] 3.523928


#test data 최종 모델에 적용
pred_test_lm <- predict(boston.data.lm, newdata = test)
pred_test_rf <- predict(RF, newdata = test)
RMSE(pred_test_lm, test$medv)
RMSE(pred_test_rf, test$medv)






