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
library(psych)


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
data <- MASS::Boston %>% na.omit()

# 상관분석 시각화
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
# Call:
#   lm(formula = medv ~ ., data = training[, -4])
# 
# Coefficients:
#   (Intercept)         crim           zn        indus          nox           rm          age  
# 31.004731    -0.110599     0.043554    -0.039881   -16.554675     4.554050    -0.010592  
# dis          rad          tax      ptratio        black        lstat  
# -1.626804     0.330975    -0.013780    -0.837632     0.008429    -0.463904  


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
#   nox         -18.309237   5.186366  -3.530 0.000483 ***
#   rm            3.846938   0.559952   6.870 3.92e-11 ***
#   age           0.002238   0.017205   0.130 0.896579    
#   dis          -1.656438   0.283072  -5.852 1.31e-08 ***
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
# [1] 13


# 수치형 변수로만 회구모형 구축하기 (잔차 확인)
par(mfrow = c(1, 3))
data_lm_full$residuals %>% hist()
data_lm_full %>% plot(which = 1)
data_lm_full %>% plot(which = 2)

# RMSE 학습
data_lm_full$fitted.values %>% MLmetrics::RMSE(training$medv)
# [1] 4.578981

# RMSE 검증
pred_data_lm_full <- data_lm_full %>% predict(newdata = test)
data_lm_full_rmse <- pred_data_lm_full %>% RMSE(test$medv)
# [1] 5.814099

# ---------------------------- #
# 이차항 선형 회귀

# 수치형 변수로만 회귀모형 구축하기 (2차 상호작용까지 고려)
data_lm_full_2 <- lm(medv ~ .^2, data = training[,-4])
# Call:
#   lm(formula = medv ~ .^2, data = training[, -4])
# 
# Coefficients:
#   (Intercept)           crim             zn          indus            nox             rm  
# -1.001e+02     -1.532e+01      4.029e-01     -1.130e+00     -5.149e+01      2.712e+01  
# age            dis            rad            tax        ptratio          black  
# 1.400e+00     -5.234e+00      7.804e-01     -2.627e-02     -1.300e+00      6.178e-02  
# lstat        crim:zn     crim:indus       crim:nox        crim:rm       crim:age  
# 2.621e+00      1.511e-01     -3.264e-02     -1.380e+00      1.315e-01     -5.034e-03  
# crim:dis       crim:rad       crim:tax   crim:ptratio     crim:black     crim:lstat  
# -1.440e-01     -5.718e-01      3.007e-02      4.836e-01     -2.822e-04      2.903e-02  
# zn:indus         zn:nox          zn:rm         zn:age         zn:dis         zn:rad  
# -2.207e-04      6.526e-01     -1.548e-02      8.676e-05      9.274e-03      5.107e-03  
# zn:tax     zn:ptratio       zn:black       zn:lstat      indus:nox       indus:rm  
# 3.577e-04      1.807e-03     -1.804e-03     -1.272e-02      2.072e+00      1.170e-01  
# indus:age      indus:dis      indus:rad      indus:tax  indus:ptratio    indus:black  
# 3.833e-03     -3.336e-02     -4.881e-02      9.615e-04     -5.951e-02      1.057e-03  
# indus:lstat         nox:rm        nox:age        nox:dis        nox:rad        nox:tax  
# -2.550e-02      5.245e+00     -4.147e-01     -6.657e-01      1.031e+00     -1.168e-01  
# nox:ptratio      nox:black      nox:lstat         rm:age         rm:dis         rm:rad  
# 1.386e+00      3.105e-02      1.339e+00     -6.996e-02      2.398e-01     -7.999e-03  
# rm:tax     rm:ptratio       rm:black       rm:lstat        age:dis        age:rad  
# -1.663e-02     -3.605e-01     -1.308e-02     -3.116e-01     -5.098e-03      7.462e-03  
# age:tax    age:ptratio      age:black      age:lstat        dis:rad        dis:tax  
# -3.536e-04     -9.153e-03     -1.336e-03     -7.475e-03     -1.647e-01     -3.758e-03  
# dis:ptratio      dis:black      dis:lstat        rad:tax    rad:ptratio      rad:black  
# 3.782e-02      9.042e-03      8.182e-02      8.289e-04      5.090e-02     -2.757e-03  
# rad:lstat    tax:ptratio      tax:black      tax:lstat  ptratio:black  ptratio:lstat  
# -4.662e-02      6.835e-03      1.882e-04     -5.615e-04      2.330e-03     -1.600e-02  
# black:lstat  
# -1.025e-03  

data_lm_full_2 %>% summary()
# Call:
#   lm(formula = medv ~ .^2, data = training[, -4])
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -9.8436 -1.6403 -0.1735  1.4063 14.0718 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   -1.001e+02  1.129e+02  -0.887  0.37607    
# crim          -1.532e+01  9.542e+00  -1.606  0.10968    
# zn             4.029e-01  8.943e-01   0.450  0.65279    
# indus         -1.130e+00  2.769e+00  -0.408  0.68364    
# nox           -5.149e+01  1.212e+02  -0.425  0.67139    
# rm             2.712e+01  1.106e+01   2.453  0.01494 *  
# age            1.400e+00  4.847e-01   2.888  0.00425 ** 
# dis           -5.234e+00  7.870e+00  -0.665  0.50670    
# rad            7.804e-01  4.058e+00   0.192  0.84769    
# tax           -2.627e-02  2.572e-01  -0.102  0.91876    
# ptratio       -1.300e+00  4.384e+00  -0.296  0.76713    
# black          6.178e-02  1.120e-01   0.551  0.58191    
# lstat          2.621e+00  1.413e+00   1.855  0.06490 .  
# crim:zn        1.511e-01  2.833e-01   0.533  0.59435    
# crim:indus    -3.264e-02  8.955e-01  -0.036  0.97095    
# crim:nox      -1.380e+00  1.441e+00  -0.957  0.33945    
# crim:rm        1.315e-01  7.609e-02   1.728  0.08532 .  
# crim:age      -5.033e-03  5.725e-03  -0.879  0.38024    
# crim:dis      -1.440e-01  1.594e-01  -0.903  0.36730    
# crim:rad      -5.718e-01  1.172e+00  -0.488  0.62622    
# crim:tax       3.007e-02  8.686e-02   0.346  0.72952    
# crim:ptratio   4.836e-01  5.284e-01   0.915  0.36110    
# crim:black    -2.822e-04  2.445e-04  -1.154  0.24959    
# crim:lstat     2.903e-02  1.044e-02   2.781  0.00589 ** 
# zn:indus      -2.207e-04  8.596e-03  -0.026  0.97954    
# zn:nox         6.526e-01  8.112e-01   0.805  0.42196    
# zn:rm         -1.548e-02  4.122e-02  -0.376  0.70750    
# zn:age         8.676e-05  1.399e-03   0.062  0.95061    
# zn:dis         9.274e-03  1.202e-02   0.772  0.44113    
# zn:rad         5.107e-03  1.078e-02   0.474  0.63615    
# zn:tax         3.577e-04  2.852e-04   1.254  0.21106    
# zn:ptratio     1.807e-03  1.038e-02   0.174  0.86195    
# zn:black      -1.804e-03  1.697e-03  -1.063  0.28903    
# zn:lstat      -1.272e-02  7.823e-03  -1.626  0.10545    
# indus:nox      2.072e+00  2.328e+00   0.890  0.37426 

# indus:rm       1.170e-01  2.232e-01   0.524  0.60073    
# indus:age      3.833e-03  5.788e-03   0.662  0.50857    
# indus:dis     -3.336e-02  9.694e-02  -0.344  0.73105    
# indus:rad     -4.881e-02  7.207e-02  -0.677  0.49897    
# indus:tax      9.615e-04  9.291e-04   1.035  0.30186    
# indus:ptratio -5.951e-02  5.747e-02  -1.035  0.30156    
# indus:black    1.057e-03  2.807e-03   0.376  0.70693    
# indus:lstat   -2.550e-02  2.996e-02  -0.851  0.39553    
# nox:rm         5.245e+00  1.022e+01   0.513  0.60827    
# nox:age       -4.147e-01  3.328e-01  -1.246  0.21404    
# nox:dis       -6.657e-01  5.436e+00  -0.122  0.90264    
# nox:rad        1.031e+00  2.926e+00   0.352  0.72482    
# nox:tax       -1.168e-01  2.053e-01  -0.569  0.56990    
# nox:ptratio    1.386e+00  4.714e+00   0.294  0.76909    
# nox:black      3.105e-02  5.450e-02   0.570  0.56947    
# nox:lstat      1.339e+00  1.284e+00   1.043  0.29812    
# rm:age        -6.996e-02  3.281e-02  -2.133  0.03405 *  
# rm:dis         2.398e-01  5.521e-01   0.434  0.66443    
# rm:rad        -7.999e-03  2.663e-01  -0.030  0.97606    
# rm:tax        -1.663e-02  1.709e-02  -0.973  0.33159    
# rm:ptratio    -3.605e-01  3.957e-01  -0.911  0.36333    
# rm:black      -1.308e-02  5.559e-03  -2.352  0.01953 *  
# rm:lstat      -3.116e-01  7.562e-02  -4.120 5.33e-05 ***
# age:dis       -5.098e-03  1.459e-02  -0.349  0.72717    
# age:rad        7.462e-03  6.960e-03   1.072  0.28484    
# age:tax       -3.536e-04  3.800e-04  -0.930  0.35312    
# age:ptratio   -9.153e-03  1.063e-02  -0.861  0.39017    
# age:black     -1.336e-03  4.085e-04  -3.270  0.00124 ** 
# age:lstat     -7.475e-03  2.950e-03  -2.534  0.01196 *  
# dis:rad       -1.647e-01  1.121e-01  -1.470  0.14304    
# dis:tax       -3.758e-03  3.923e-03  -0.958  0.33913    
# dis:ptratio    3.782e-02  1.707e-01   0.222  0.82484    
# dis:black      9.042e-03  1.055e-02   0.857  0.39214    
# dis:lstat      8.182e-02  8.367e-02   0.978  0.32917    
# rad:tax        8.289e-04  2.023e-03   0.410  0.68243    
# rad:ptratio    5.090e-02  1.256e-01   0.405  0.68558    
# rad:black     -2.757e-03  4.651e-03  -0.593  0.55399    
# rad:lstat     -4.662e-02  3.475e-02  -1.342  0.18107    
# tax:ptratio    6.835e-03  3.683e-03   1.856  0.06480 .  
# tax:black      1.882e-04  3.694e-04   0.509  0.61091    
# tax:lstat     -5.615e-04  2.514e-03  -0.223  0.82350    
# ptratio:black  2.330e-03  4.906e-03   0.475  0.63532    
# ptratio:lstat -1.600e-02  5.199e-02  -0.308  0.75849    
# black:lstat   -1.025e-03  6.205e-04  -1.652  0.09987 .  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 3.209 on 224 degrees of freedom
# Multiple R-squared:  0.9088,	Adjusted R-squared:  0.8771 
# F-statistic: 28.63 on 78 and 224 DF,  p-value: < 2.2e-16


# 계수 개수확인
data_lm_full_2 %>% length()
# [1] 12

# 잔차 확인하기
data_lm_full_2$residuals %>% hist()
data_lm_full_2 %>% plot(which = 1)
data_lm_full_2 %>% plot(which = 2)

# RMSE 학습
data_lm_full_2$fitted.values %>% MLmetrics::RMSE(training$medv)
# [1] 2.759226

# RMSE 검증
pred_data_lm_full_2 <- data_lm_full_2 %>% predict(newdata = test)
data_lm_full_2_rmse <- pred_data_lm_full_2 %>% RMSE(test$medv)
# [1] 4.116881


# ---------------------------- #
# both

# 변수선택법 활용하기(stepAIC 함수 이용하기 (both방법))
data_step_both <- data_lm_full %>% MASS::stepAIC(direction = "both",
                               scope = list(
                                 upper = ~.^2,
                                 lower = ~1
                               ))
# Call:
#   lm(formula = medv ~ crim + indus + nox + rm + age + dis + rad + 
#        tax + ptratio + black + lstat + rm:lstat + rad:lstat + dis:rad + 
#        black:lstat + rm:rad + nox:rad + age:black + nox:age + nox:dis + 
#        indus:lstat + indus:age + indus:tax + rm:ptratio + age:tax, 
#      data = training[, -4])
# 
# Coefficients:
#   (Intercept)         crim        indus          nox           rm          age          dis          rad  
# -1.370e+02   -1.708e-01   -5.032e-01    5.687e+01    1.733e+01    5.604e-01    3.752e+00    3.127e+00  
# tax      ptratio        black        lstat     rm:lstat    rad:lstat      dis:rad  black:lstat  
# -1.556e-02    1.324e+00    1.106e-01    3.324e+00   -4.038e-01   -4.681e-02   -2.236e-01   -1.270e-03  
# rm:rad      nox:rad    age:black      nox:age      nox:dis  indus:lstat    indus:age    indus:tax  
# -1.740e-01   -6.297e-01   -9.409e-04   -5.086e-01   -8.312e+00   -3.019e-02    7.637e-03    1.001e-03  
# rm:ptratio      age:tax  
# -2.996e-01   -2.292e-04  


data_step_both %>% summary()
# Call:
#   lm(formula = medv ~ crim + indus + nox + rm + age + dis + rad + 
#        tax + ptratio + black + lstat + rm:lstat + rad:lstat + dis:rad + 
#        black:lstat + rm:rad + nox:rad + age:black + nox:age + nox:dis + 
#        indus:lstat + indus:age + indus:tax + rm:ptratio + age:tax, 
#      data = training[, -4])
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -12.6400  -1.7780  -0.2218   1.4254  15.7804 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -1.370e+02  2.282e+01  -6.005 5.99e-09 ***
#   crim        -1.708e-01  2.696e-02  -6.336 9.50e-10 ***
#   indus       -5.032e-01  2.236e-01  -2.251 0.025187 *  
#   nox          5.687e+01  2.002e+01   2.841 0.004836 ** 
#   rm           1.733e+01  2.372e+00   7.304 2.99e-12 ***
#   age          5.604e-01  1.561e-01   3.590 0.000390 ***
#   dis          3.752e+00  1.160e+00   3.233 0.001373 ** 
#   rad          3.127e+00  5.019e-01   6.229 1.73e-09 ***
#   tax         -1.556e-02  7.364e-03  -2.112 0.035540 *  
#   ptratio      1.324e+00  9.373e-01   1.413 0.158876    
#   black        1.106e-01  2.393e-02   4.623 5.80e-06 ***
#   lstat        3.324e+00  3.538e-01   9.394  < 2e-16 ***
#   rm:lstat    -4.038e-01  4.959e-02  -8.143 1.32e-14 ***
#   rad:lstat   -4.681e-02  5.582e-03  -8.386 2.59e-15 ***
#   dis:rad     -2.236e-01  3.900e-02  -5.733 2.57e-08 ***
#   black:lstat -1.270e-03  4.093e-04  -3.102 0.002117 ** 
#   rm:rad      -1.740e-01  5.996e-02  -2.902 0.004001 ** 
#   nox:rad     -6.297e-01  3.917e-01  -1.608 0.109040    
#   age:black   -9.409e-04  2.843e-04  -3.310 0.001058 ** 
#   nox:age     -5.086e-01  1.837e-01  -2.769 0.006001 ** 
#   nox:dis     -8.312e+00  2.664e+00  -3.120 0.001998 ** 
#   indus:lstat -3.019e-02  8.267e-03  -3.652 0.000311 ***
#   indus:age    7.637e-03  2.438e-03   3.133 0.001916 ** 
#   indus:tax    1.001e-03  3.661e-04   2.734 0.006661 ** 
#   rm:ptratio  -2.996e-01  1.444e-01  -2.076 0.038849 *  
#   age:tax     -2.292e-04  1.220e-04  -1.879 0.061302 .  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 3.17 on 277 degrees of freedom
# Multiple R-squared:   0.89,	Adjusted R-squared:   0.88 
# F-statistic: 89.61 on 25 and 277 DF,  p-value: < 2.2e-16


# 계수 개수 확인
data_step_both %>% 
  stats::coef() %>% 
  base::length()
# [1] 26

# 잔차 확인하기
data_step_both$residuals %>% hist()
data_step_both %>% plot(which = 1)
data_step_both %>% plot(which = 2)

# RMSE 학습
data_step_both$fitted.values %>% MLmetrics::RMSE(training$medv)
# [1] 2.759226

# RMSE 검증
pred_data_step_both <- data_step_both %>% predict(newdata = test)
pred_data_step_both %>% RMSE(test$medv)
# [1] 4.451526


# ---------------------------- #
# forward

# 변수선택법 활용하기(stepAIC 함수 이용하기 (forward방법))
data_step_forward <- data_lm_full %>% MASS::stepAIC(direction = "forward",
                                                 scope = list(
                                                   upper = ~.^2,
                                                   lower = ~1
                                                 ))
# Call:
#   lm(formula = medv ~ crim + zn + indus + nox + rm + age + dis + 
#        rad + tax + ptratio + black + lstat + rm:lstat + rad:lstat + 
#        dis:rad + black:lstat + rm:rad + nox:rad + age:black + nox:age + 
#        nox:dis + indus:lstat + indus:age + indus:tax + rm:ptratio + 
#        age:tax + zn:rad + dis:ptratio + zn:dis + rm:age + age:lstat + 
#        crim:lstat + crim:rm + rm:black, data = training[, -4])
# 
# Coefficients:
#   (Intercept)         crim           zn        indus          nox           rm          age          dis  
# -1.648e+02   -1.297e+00   -1.106e-01   -4.928e-01    2.687e+01    2.440e+01    8.745e-01   -1.087e+00  
# rad          tax      ptratio        black        lstat     rm:lstat    rad:lstat      dis:rad  
# 3.918e+00   -1.289e-02    1.204e+00    1.647e-01    3.235e+00   -3.432e-01   -5.890e-02   -3.027e-01  
# black:lstat       rm:rad      nox:rad    age:black      nox:age      nox:dis  indus:lstat    indus:age  
# -1.199e-03   -2.436e-01   -5.787e-01   -1.065e-03   -3.311e-01   -3.266e+00   -2.416e-02    6.744e-03  
# indus:tax   rm:ptratio      age:tax       zn:rad  dis:ptratio       zn:dis       rm:age    age:lstat  
# 1.048e-03   -3.677e-01   -2.569e-04    1.384e-02    1.419e-01    1.155e-02   -4.715e-02   -4.214e-03  
# crim:lstat      crim:rm     rm:black  
# 2.110e-02    1.137e-01   -7.303e-03  


data_step_forward %>% summary()
# Call:
#   lm(formula = medv ~ crim + zn + indus + nox + rm + age + dis + 
#        rad + tax + ptratio + black + lstat + rm:lstat + rad:lstat + 
#        dis:rad + black:lstat + rm:rad + nox:rad + age:black + nox:age + 
#        nox:dis + indus:lstat + indus:age + indus:tax + rm:ptratio + 
#        age:tax + zn:rad + dis:ptratio + zn:dis + rm:age + age:lstat + 
#        crim:lstat + crim:rm + rm:black, data = training[, -4])
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -11.0684  -1.7355  -0.2457   1.4623  14.7177 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -1.648e+02  2.985e+01  -5.521 7.95e-08 ***
#   crim        -1.297e+00  5.150e-01  -2.519 0.012353 *  
#   zn          -1.106e-01  4.729e-02  -2.339 0.020048 *  
#   indus       -4.928e-01  2.372e-01  -2.078 0.038664 *  
#   nox          2.687e+01  2.223e+01   1.209 0.227846    
# rm           2.440e+01  3.514e+00   6.941 2.92e-11 ***
#   age          8.745e-01  2.068e-01   4.228 3.23e-05 ***
#   dis         -1.087e+00  2.081e+00  -0.522 0.602034    
# rad          3.918e+00  6.943e-01   5.644 4.23e-08 ***
#   tax         -1.289e-02  7.648e-03  -1.685 0.093165 .  
# ptratio      1.204e+00  1.007e+00   1.196 0.232906    
# black        1.647e-01  4.165e-02   3.954 9.84e-05 ***
#   lstat        3.235e+00  4.078e-01   7.931 5.90e-14 ***
#   rm:lstat    -3.432e-01  5.480e-02  -6.262 1.50e-09 ***
#   rad:lstat   -5.890e-02  7.811e-03  -7.541 7.24e-13 ***
#   dis:rad     -3.027e-01  4.701e-02  -6.440 5.50e-10 ***
#   black:lstat -1.199e-03  4.531e-04  -2.645 0.008642 ** 
#   rm:rad      -2.436e-01  8.432e-02  -2.890 0.004174 ** 
#   nox:rad     -5.787e-01  4.075e-01  -1.420 0.156761    
# age:black   -1.065e-03  2.899e-04  -3.674 0.000288 ***
#   nox:age     -3.311e-01  1.959e-01  -1.690 0.092177 .  
# nox:dis     -3.266e+00  3.281e+00  -0.995 0.320471    
# indus:lstat -2.416e-02  8.538e-03  -2.830 0.005003 ** 
#   indus:age    6.744e-03  2.596e-03   2.599 0.009881 ** 
#   indus:tax    1.048e-03  3.628e-04   2.889 0.004182 ** 
#   rm:ptratio  -3.677e-01  1.522e-01  -2.416 0.016344 *  
#   age:tax     -2.569e-04  1.238e-04  -2.076 0.038874 *  
#   zn:rad       1.384e-02  6.974e-03   1.985 0.048215 *  
#   dis:ptratio  1.419e-01  6.362e-02   2.231 0.026515 *  
#   zn:dis       1.155e-02  6.173e-03   1.872 0.062345 .  
# rm:age      -4.715e-02  1.931e-02  -2.442 0.015251 *  
#   age:lstat   -4.214e-03  2.102e-03  -2.005 0.046008 *  
#   crim:lstat   2.110e-02  8.253e-03   2.557 0.011124 *  
#   crim:rm      1.137e-01  6.046e-02   1.880 0.061169 .  
# rm:black    -7.303e-03  4.483e-03  -1.629 0.104505    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 3.105 on 268 degrees of freedom
# Multiple R-squared:  0.8979,	Adjusted R-squared:  0.885 
# F-statistic: 69.33 on 34 and 268 DF,  p-value: < 2.2e-16


# 계수 개수 확인
data_step_forward %>% 
  stats::coef() %>% 
  base::length()
# [1] 35

# 잔차 확인하기
data_step_forward$residuals %>% hist()
data_step_forward %>% plot(which = 1)
data_step_forward %>% plot(which = 2)


# RMSE 학습
data_step_forward$fitted.values %>% MLmetrics::RMSE(training$medv)
# [1] 2.919732

# RMSE 검증
pred_data_step_forward <- data_step_forward %>% predict(newdata = test)
pred_data_step_forward %>% RMSE(test$medv)
# [1] 4.226292

# ---------------------------- #
# backward

# 변수선택법 활용하기(stepAIC 함수 이용하기 (both방법))
data_step_backward <- data_lm_full %>% MASS::stepAIC(direction = "backward",
                                                    scope = list(
                                                      upper = ~.^2,
                                                      lower = ~1
                                                    ))
# Call:
#   lm(formula = medv ~ crim + zn + nox + rm + dis + rad + tax + 
#        ptratio + black + lstat, data = training[, -4])
# 
# Coefficients:
#   (Intercept)         crim           zn          nox           rm          dis          rad          tax  
# 31.488634    -0.109549     0.045632   -18.078568     4.516123    -1.550658     0.347991    -0.014984  
# ptratio        black        lstat  
# -0.856037     0.008326    -0.478895 

data_step_backward %>% summary()
# Call:
#   lm(formula = medv ~ crim + zn + nox + rm + dis + rad + tax + 
#        ptratio + black + lstat, data = training[, -4])
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -11.1437  -2.5168  -0.6277   1.7997  26.9886 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  31.488634   6.490285   4.852 2.00e-06 ***
#   crim         -0.109549   0.036056  -3.038 0.002594 ** 
#   zn            0.045632   0.017390   2.624 0.009146 ** 
#   nox         -18.078568   4.407141  -4.102 5.31e-05 ***
#   rm            4.516123   0.535383   8.435 1.55e-15 ***
#   dis          -1.550658   0.237295  -6.535 2.83e-10 ***
#   rad           0.347991   0.081178   4.287 2.47e-05 ***
#   tax          -0.014984   0.004493  -3.335 0.000963 ***
#   ptratio      -0.856037   0.161663  -5.295 2.34e-07 ***
#   black         0.008326   0.003479   2.393 0.017333 *  
#   lstat        -0.478895   0.064858  -7.384 1.62e-12 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 4.67 on 292 degrees of freedom
# Multiple R-squared:  0.7484,	Adjusted R-squared:  0.7397 
# F-statistic: 86.84 on 10 and 292 DF,  p-value: < 2.2e-16


# 계수 개수 확인
data_step_backward %>% 
  stats::coef() %>% 
  base::length()
# [1] 11

# 잔차 확인하기
data_step_backward$residuals %>% hist()
data_step_backward %>% plot(which = 1)
data_step_backward %>% plot(which = 2)

# RMSE 학습
data_step_backward$fitted.values %>% MLmetrics::RMSE(training$medv)
# [1] 4.584082

# RMSE 검증
pred_data_step_backward <- data_step_backward %>% predict(newdata = test)
pred_data_step_backward %>% RMSE(test$medv)
# [1] 5.77352


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