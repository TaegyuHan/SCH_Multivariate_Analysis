# ---------------------------------------------------------------- #
# 2022-10-06 (목요일) 6주 12차 다변량 분석 강의
# 내용 : 
#                                                 - 김재윤 교수님 -  
# ---------------------------------------------------------------- #


library(tidyverse)
library(MASS)


# ---------------------------------------------------------------- #
# 과제는 최대한 기말고사 전 까지 제출해주면 감점이 없다.
# 제출했냐 안했냐가 중요!!


# Residuals vs Fitted의 그래프를 가지고
# 1개의 잔차 그래프를 가지고
# 선형
# 독립성
# 등분산성
# 정규성을 볼 수 있다.
# 
# 패턴없이 0주변에 랜덤하게 몰려있어야 한다.
# 수평선 주위의 랜덤한 형태로 나타나지 않음
# 등분산성 만족하지 못함.

# ---------------------------------------------------------------- #
women.lm.mul2<- lm(women$weight ~ women$height^2)
# Call:
#   lm(formula = women$weight ~ women$height + (women$height)^2)
# 
# Coefficients:
#   (Intercept)  women$height  
# -87.52          3.45

summary(women.lm.mul2)
# Call:
#   lm(formula = women$weight ~ women$height + (women$height)^2)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -1.7333 -1.1333 -0.3833  0.7417  3.1167 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  -87.51667    5.93694  -14.74 1.71e-09 ***
#   women$height   3.45000    0.09114   37.85 1.09e-14 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1.525 on 13 degrees of freedom
#   Multiple R-squared:  0.991,	Adjusted R-squared:  0.9903 
#   F-statistic:  1433 on 1 and 13 DF,  p-value: 1.091e-14

plot(women.lm.mul2, which = 1)

# Q-Q plot
plot(women.lm.mul2, which = 2)





# ---------------------------------------------------------------- #
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

# 데이터의 정규분포가 편향되어 있을 때 
# log를 사용하여 정규분포를 만들어준다.
airquality.lm.log <- lm(log(airquality$Ozone)
                        ~ airquality$Solar.R 
                        + airquality$Wind 
                        + airquality$Temp)
# Call:
#   lm(formula = log(airquality$Ozone) ~ airquality$Solar.R + airquality$Wind + 
#        airquality$Temp)
# 
# Coefficients:
#   (Intercept)  airquality$Solar.R     airquality$Wind     airquality$Temp  
# -0.262132            0.002515           -0.061562            0.049171  

summary(airquality.lm.log)
# Call:
#   lm(formula = log(airquality$Ozone) ~ airquality$Solar.R + airquality$Wind + 
#        airquality$Temp)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -2.06193 -0.29970 -0.00231  0.30756  1.23578 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)        -0.2621323  0.5535669  -0.474 0.636798    
# airquality$Solar.R  0.0025152  0.0005567   4.518 1.62e-05 ***
#   airquality$Wind    -0.0615625  0.0157130  -3.918 0.000158 ***
#   airquality$Temp     0.0491711  0.0060875   8.077 1.07e-12 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.5086 on 107 degrees of freedom
# Multiple R-squared:  0.6644,	Adjusted R-squared:  0.655 
# F-statistic: 70.62 on 3 and 107 DF,  p-value: < 2.2e-16

# 보스턴 주택 가격 데이터 과제

# 전체 데이터를 가지고
# 데이터 타입
# 기초 통계량
# 시각화
# 탐색적 데이터 분석 실시

# 수치형 변수들 간의 상관 분석 진행
#   - 종속 변수와 독립 변수들 간의 관계를 파악
#   - 독립 변수들 간의 관계 파악

# 모형 평가 ( 학습과 검증 데이터로 판단 )
#   - 회귀 모형 적합성, 
#   - 회귀계수의 유의성 검정, 
#   - 잔차평가 (테스트 데이터를 사용하지 말고)
#   - 정확도(RMSE) 평가등

# 다양한 회귀 모형과 비교분석
#   - 이차항을 고려한 선형 회귀,
#   - 나무 모형,
#   - 랜덤 포레스트,
#   - 부스팅

# ---------- 데이터 타입 ---------- #
MASS::Boston %>% str()
# 'data.frame':	506 obs. of  14 variables:
# $ crim   : num  0.00632 0.02731 0.02729 0.03237 0.06905 ...
# $ zn     : num  18 0 0 0 0 0 12.5 12.5 12.5 12.5 ...
# $ indus  : num  2.31 7.07 7.07 2.18 2.18 2.18 7.87 7.87 7.87 7.87 ...
# $ chas   : int  0 0 0 0 0 0 0 0 0 0 ... < 범주형 데이터 제외
# $ nox    : num  0.538 0.469 0.469 0.458 0.458 0.458 0.524 0.524 0.524 0.524 ...
# $ rm     : num  6.58 6.42 7.18 7 7.15 ...
# $ age    : num  65.2 78.9 61.1 45.8 54.2 58.7 66.6 96.1 100 85.9 ...
# $ dis    : num  4.09 4.97 4.97 6.06 6.06 ...
# $ rad    : int  1 2 2 3 3 3 5 5 5 5 ...
# $ tax    : num  296 242 242 222 222 222 311 311 311 311 ...
# $ ptratio: num  15.3 17.8 17.8 18.7 18.7 18.7 15.2 15.2 15.2 15.2 ...
# $ black  : num  397 397 393 395 397 ...
# $ lstat  : num  4.98 9.14 4.03 2.94 5.33 ...
# $ medv   : num  24 21.6 34.7 33.4 36.2 28.7 22.9 27.1 16.5 18.9 ... # 타겟 데이터

MASS::Boston %>% is.na() %>% sum()
# [1] 0

# ---------- 기초 통계량 ---------- #

MASS::Boston %>% names()
# [1] "crim"    "zn"      "indus"   "chas"    "nox"     "rm"      "age"     "dis"     "rad"    
# [10] "tax"     "ptratio" "black"   "lstat"   "medv"

MASS::Boston %>% chas

#       crim                zn             indus            chas              nox        
# Min.   : 0.00632   Min.   :  0.00   Min.   : 0.46   Min.   :0.00000   Min.   :0.3850  
# 1st Qu.: 0.08205   1st Qu.:  0.00   1st Qu.: 5.19   1st Qu.:0.00000   1st Qu.:0.4490  
# Median : 0.25651   Median :  0.00   Median : 9.69   Median :0.00000   Median :0.5380  
# Mean   : 3.61352   Mean   : 11.36   Mean   :11.14   Mean   :0.06917   Mean   :0.5547  
# 3rd Qu.: 3.67708   3rd Qu.: 12.50   3rd Qu.:18.10   3rd Qu.:0.00000   3rd Qu.:0.6240  
# Max.   :88.97620   Max.   :100.00   Max.   :27.74   Max.   :1.00000   Max.   :0.8710  
#       rm             age              dis              rad              tax       
# Min.   :3.561   Min.   :  2.90   Min.   : 1.130   Min.   : 1.000   Min.   :187.0  
# 1st Qu.:5.886   1st Qu.: 45.02   1st Qu.: 2.100   1st Qu.: 4.000   1st Qu.:279.0  
# Median :6.208   Median : 77.50   Median : 3.207   Median : 5.000   Median :330.0  
# Mean   :6.285   Mean   : 68.57   Mean   : 3.795   Mean   : 9.549   Mean   :408.2  
# 3rd Qu.:6.623   3rd Qu.: 94.08   3rd Qu.: 5.188   3rd Qu.:24.000   3rd Qu.:666.0  
# Max.   :8.780   Max.   :100.00   Max.   :12.127   Max.   :24.000   Max.   :711.0  
#     ptratio          black            lstat            medv      
# Min.   :12.60   Min.   :  0.32   Min.   : 1.73   Min.   : 5.00  
# 1st Qu.:17.40   1st Qu.:375.38   1st Qu.: 6.95   1st Qu.:17.02  
# Median :19.05   Median :391.44   Median :11.36   Median :21.20  
# Mean   :18.46   Mean   :356.67   Mean   :12.65   Mean   :22.53  
# 3rd Qu.:20.20   3rd Qu.:396.23   3rd Qu.:16.95   3rd Qu.:25.00  
# Max.   :22.00   Max.   :396.90   Max.   :37.97   Max.   :50.00 











