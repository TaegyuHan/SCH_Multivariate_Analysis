# ---------------------------------------------------------------- #
# 2022-10-11 (화요일) 7주 13차 다변량 분석 강의
# 내용 : 
#                                                 - 김재윤 교수님 -  
# ---------------------------------------------------------------- #

# ---------------------------------------------------------------- #
# Package
# install.packages("MLmetrics")
library(MASS)
library(tidyverse)
library(MLmetrics)


# ---------------------------------------------------------------- #
# 과제를 꼭 제출해 주세요!
# 이번주 내로 제출을 해주세요!

# 상관관계를 알아보기전에
# 이상치를 확인해야 한다.


# ---------------------------------------------------------------- #
# 변수 선택법 활용하기

# 가장 많이 사용하는 방법(both)
# 1. both : 전진선택법과 후진제거법을 번갈아가면서 수행

# 좋은 것만 찾아가는 알고리즘 이다.
# 2. backward(후진제거법) : 모든 변수가 포함된 모형에서 유의하지 않은
#     변수를 제거
# 3. Forward(전진선택법) : 가장 유의한 변수들부터 하나씩 추가

# AIC : 낮을 수록 설명력이 좋은 모델이다.


# 데이터
data <- MASS::Boston

# 상관분석 시각화
library(psych)
psych::pairs.panels(data[,-4])


# 데이터 나누기
set.seed(1606)

n <- data %>% nrow()
idx <- 1:n

# 데이터 나누기
training_idx <- idx %>% sample(n * .60)
idx <- idx %>% setdiff(training_idx)
validate_idx <- idx %>% sample(n * .20)
test_idx <- idx %>% setdiff(validate_idx)
training <- data[training_idx,]
validation <- data[validate_idx,]
test <- data[test_idx,]


# ---------------------------- #
# 선형 회귀

# 수치형 변수로만 회귀 모형 구축하기(1차항만 고려)
data_lm_full <- lm(medv ~ ., data = training[,-4])

data_lm_full %>% summary()
# Call:
#   lm(formula = medv ~ ., data = training[, -4])
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -13.1808  -2.7861  -0.5977   1.7613  25.8443 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  37.494208   6.765003   5.542 6.71e-08 ***
#   crim         -0.092617   0.043024  -2.153 0.032171 *  
#   zn            0.052362   0.019119   2.739 0.006549 ** 
#   indus         0.100176   0.079628   1.258 0.209381    
# nox         -18.309237   5.186366  -3.530 0.000483 ***
#   rm            3.846938   0.559952   6.870 3.92e-11 ***
#   age           0.002238   0.017205   0.130 0.896579    
# dis          -1.656438   0.283072  -5.852 1.31e-08 ***
#   rad           0.332811   0.085817   3.878 0.000130 ***
#   tax          -0.015654   0.004787  -3.270 0.001206 ** 
#   ptratio      -1.017756   0.174730  -5.825 1.52e-08 ***
#   black         0.011679   0.003532   3.307 0.001063 ** 
#   lstat        -0.512180   0.064877  -7.895 6.03e-14 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 4.875 on 290 degrees of freedom
# Multiple R-squared:  0.7322,(< 결정 계수)	
# Adjusted R-squared:  0.7211 
# F-statistic: 66.08 on 12 and 290 DF,  p-value: < 2.2e-16


# 계수 개수 확인
data_lm_full %>% 
  stats::coef() %>% 
  base::length()


# 수치형 변수로만 회구모형 구축하기 (잔차 확인)
data_lm_full$residuals %>% hist()
data_lm_full %>% plot(which = 1)
data_lm_full %>% plot(which = 2)

# RMSE 학습
data_lm_full$fitted.values %>% MLmetrics::RMSE(training$medv)

# RMSE 검증
data_lm_full %>% stats::predict()


# ---------------------------- #
# 이차항 선형 회귀

# 수치형 변수로만 회귀모형 구축하기 (2차 상호작용까지 고려)
data_lm_full_2 <- lm(medv ~ .^2, data = training[,-4])
data_lm_full_2 %>% summary()

# 계수 개수확인
data_lm_full_2 %>% length()

# 잔차 확인하기
data_lm_full_2$residuals %>% hist()
data_lm_full_2 %>% plot(which = 1)
data_lm_full_2 %>% plot(which = 2)


# ---------------------------- #
# both

# 변수선택법 활용하기(stepAIC 함수 이용하기 (both방법))
data_step_both <- data_lm_full %>% MASS::stepAIC(direction = "both",
                               scope = list(
                                 upper = ~.^2,
                                 lower = ~1
                               ))

data_step_both %>% summary()

# 계수 개수 확인
data_step_both %>% 
  stats::coef() %>% 
  base::length()


# 잔차 확인하기
data_step_both$residuals %>% hist()
data_step_both %>% plot(which = 1)
data_step_both %>% plot(which = 2)


# ---------------------------- #
# forward

# 변수선택법 활용하기(stepAIC 함수 이용하기 (both방법))
data_step_forward <- data_lm_full %>% MASS::stepAIC(direction = "forward",
                                                 scope = list(
                                                   upper = ~.^2,
                                                   lower = ~1
                                                 ))

data_step_forward %>% summary()

# 계수 개수 확인
data_step_forward %>% 
  stats::coef() %>% 
  base::length()


# 잔차 확인하기
data_step_forward$residuals %>% hist()
data_step_forward %>% plot(which = 1)
data_step_forward %>% plot(which = 2)


# ---------------------------- #
# backward

# 변수선택법 활용하기(stepAIC 함수 이용하기 (both방법))
data_step_backward <- data_lm_full %>% MASS::stepAIC(direction = "backward",
                                                    scope = list(
                                                      upper = ~.^2,
                                                      lower = ~1
                                                    ))

data_step_backward %>% summary()

# 계수 개수 확인
data_step_backward %>% 
  stats::coef() %>% 
  base::length()


# 잔차 확인하기
data_step_backward$residuals %>% hist()
data_step_backward %>% plot(which = 1)
data_step_backward %>% plot(which = 2)


# 과제 목요일 전까지 하기
# 모형 평가 (학습데이터)

# (표로 나타내기)
# 계수 개수
# 결정 계수
# 수정 결정 계수
# RMSE 학습데이터
# RMSE 검증데이터

# 비교 모형
#     선형회귀모형
#     2차 상호작용까지 포함한 선형회귀모형
#     변수선택을 고려한 선형회귀모형 (stepwise)
#     변수선택을 고려한 선형회귀모형 (forward)
#     변수선택을 고려한 선형회귀모형 (backward)

# 각 모형별 잔차 분석
#     각 학습과 검증 데이터의 잔차 분석 
#     (히스토그램, Q-Q plot, 잔차와 적합된 값 산점도)

# 어떤 모형이 가장 예측률이 높은지 확인해야 한다.


# ---------------------------- #
# 다중선형회귀분석
# install.packages("carData")
library(carData)
# library(carData)

# 설명력이 높은 모형 구축하기 (변수 선택방법 적용하어 비교하기)
# 종속변수는 income, 독립변수는 education, women, prestige, census
# 결정계수 지표를 바탕으로해서 비교한다. (중간고사 시험문제!!)
prestige_data <- carData::Prestige

# 데이터 확인하기
?carData::Prestige

# type 열은 제외 한다.
prestige_data %>% str()