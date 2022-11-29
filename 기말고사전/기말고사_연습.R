# ------------------------------------------------------------------ #
# 데이터 분석 진행
# ------------------------------------------------------------------ #
# install.packages("faraway")

library(faraway)
data(sat)

str(sat)
# 'data.frame':	50 obs. of  7 variables:
# $ expend: num  4.41 8.96 4.78 4.46 4.99 ...
# $ ratio : num  17.2 17.6 19.3 17.1 24 18.4 14.4 16.6 19.1 16.3 ...
# $ salary: num  31.1 48 32.2 28.9 41.1 ...
# $ takers: int  8 47 27 6 45 29 81 68 48 65 ...
# $ verbal: int  491 445 448 482 417 462 431 429 420 406 ...
# $ math  : int  538 489 496 523 485 518 477 468 469 448 ...
# $ 종속 변수 > total : int  1029 934 944 1005 902 980 908 897 889 854 ...


# 1)
# 물음에 답하시오 (주어진 데이터를 학습데이터로 사용)
#   - 변수들 간으이 산점도를 작성 (상관계수 포함)하고 분석하시오.
#       - 종속 변수와 독립변수들과의 관계, 
#           독립변수들간의 관계(다중공선성 존재 유무)를 서술하시오.

# Plotting the correlation matrix
# install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)

PerformanceAnalytics::chart.Correlation(sat, histogram = TRUE, pch = 15)

summary(sat)
# 통계성 유의 성을 확인인
# expend          ratio           salary          takers     
# Min.   :3.656   Min.   :13.80   Min.   :25.99   Min.   : 4.00  
# 1st Qu.:4.882   1st Qu.:15.22   1st Qu.:30.98   1st Qu.: 9.00  
# Median :5.768   Median :16.60   Median :33.29   Median :28.00  
# Mean   :5.905   Mean   :16.86   Mean   :34.83   Mean   :35.24  
# 3rd Qu.:6.434   3rd Qu.:17.57   3rd Qu.:38.55   3rd Qu.:63.00  
# Max.   :9.774   Max.   :24.30   Max.   :50.05   Max.   :81.00  
# verbal           math           total       
# Min.   :401.0   Min.   :443.0   Min.   : 844.0  
# 1st Qu.:427.2   1st Qu.:474.8   1st Qu.: 897.2  
# Median :448.0   Median :497.5   Median : 945.5  
# Mean   :457.1   Mean   :508.8   Mean   : 965.9  
# 3rd Qu.:490.2   3rd Qu.:539.5   3rd Qu.:1032.0  
# Max.   :516.0   Max.   :592.0   Max.   :1107.0  

car::vif(lm(total~., data = sat))
# expend     ratio    salary     takers     verbal      math 
# 9.550753  2.565748  9.332712  7.993656 22.699303 19.574415

 

# install.packages("car")
library(car)
data_lm <- lm(total ~ ., data = sat)
car::vif(data_lm)
#   expend     ratio    salary    takers    verbal      math 
# 9.550753  2.565748  9.332712  7.993656 22.699303 19.574415


summary(data_lm)
# Call:
#   lm(formula = total ~ ., data = sat)
# 
# Residuals:
#   Min         1Q     Median         3Q        Max 
# -4.835e-13 -1.958e-13 -5.870e-14  1.160e-14  3.951e-12 
# 
# Coefficients:
#               Estimate Std. Error    t value Pr(>|t|)    
# (Intercept)  1.458e-12  3.356e-12  4.340e-01    0.666    
# expend      -3.188e-13  2.068e-13 -1.542e+00    0.131    
# ratio       -7.596e-14  6.445e-14 -1.179e+00    0.245    
# salary       7.107e-14  4.689e-14  1.516e+00    0.137    
# takers      -5.427e-15  9.633e-15 -5.630e-01    0.576    
# verbal       1.000e+00  1.235e-14  8.097e+13   <2e-16 ***
# math         1.000e+00  1.003e-14  9.966e+13   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 6.383e-13 on 43 degrees of freedom
# Multiple R-squared:      1,	Adjusted R-squared:      1 
# F-statistic: 1.122e+29 on 6 and 43 DF,  p-value: < 2.2e-16


# 2)
# total 종속변수를 가장 잘 설명할 수 있는 변수들을 선택(또는 추출)하고, 
#      그 이유를 설명하시오
#   - 주성분분석, 변수선택, 회귀계수의 유의성 검정 등 다양한 방법 중 한가지
#     선택하고 그 이유를 설명

# 주성분 분석 실시
pca_data <- prcomp(sat, center = TRUE, scale. = TRUE)
# Standard deviations (1, .., p=7):
# [1] 2.117530e+00 1.166194e+00 9.875820e-01 3.230023e-01 2.262021e-01 1.588767e-01
# [7] 1.163395e-16
# 
# Rotation (n x k) = (7 x 7):
#   PC1         PC2         PC3          PC4         PC5          PC6
# expend  0.31225604 -0.61517695  0.13839512 -0.262517017 -0.65841054 -0.049806209
# ratio  -0.09140083  0.43736911  0.84599728  0.035456257 -0.28462194 -0.048743798
# salary  0.32507484 -0.44322011  0.49154369 -0.008527291  0.67540141 -0.005224782
# takers  0.45191599  0.01719957 -0.05292280  0.874420808 -0.15711646 -0.058120164
# verbal -0.44597625 -0.25579609  0.03847703  0.191289915  0.01545975 -0.742268966
# math   -0.43463007 -0.29874801  0.11383039  0.270527610 -0.06115034  0.663863489
# total  -0.44321803 -0.28079117  0.07925610  0.235300205 -0.02559085  0.007757693
# PC7
# expend  0.000000e+00
# ratio   1.427983e-16
# salary -3.050315e-16
# takers  2.335668e-16
# verbal -3.826212e-01
# math   -4.373210e-01
# total   8.138497e-01

summary(pca_data)
# Importance of components:
#                           PC1    PC2    PC3    PC4     PC5     PC6       PC7
# Standard deviation     2.1175 1.1662 0.9876 0.3230 0.22620 0.15888 1.163e-16
# Proportion of Variance 0.6406 0.1943 0.1393 0.0149 0.00731 0.00361 0.000e+00
# Cumulative Proportion  0.6406 0.8348 0.9742 0.9891 0.99639 1.00000 1.000e+00


# 3)
# 2)을 통해 선정된 변수(또는 주성분)들을 바탕으로 회귀식을 적합하고, 모형 적합성
#     및 회귀계수의 통계적 검정과 결과를 해석하시오.
library(MASS)

data_step_both <- MASS::stepAIC(data_lm,
                                direction = "both",
                                scope = list(upper = ~., lower = ~1))
# Call:
#   lm(formula = total ~ verbal + math, data = sat)
# 
# Coefficients:
#   (Intercept)       verbal         math  
# -5.145e-13    1.000e+00    1.000e+00  

summary(data_step_both)
# Call:
#   lm(formula = total ~ verbal + math, data = sat)
# 
# Residuals:
#        Min         1Q     Median         3Q        Max 
# -2.862e-13 -1.882e-13 -7.120e-14 -2.840e-14  4.200e-12 
# 
# Coefficients:
#               Estimate Std. Error    t value Pr(>|t|)    
# (Intercept) -5.145e-13  1.173e-12 -4.390e-01    0.663    
# verbal       1.000e+00  1.057e-14  9.462e+13   <2e-16 ***
# math         1.000e+00  9.247e-15  1.081e+14   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 6.3e-13 on 47 degrees of freedom
# Multiple R-squared:      1,	Adjusted R-squared:      1 
# F-statistic: 3.456e+29 on 2 and 47 DF,  p-value: < 2.2e-16

# 4)
# 3)을 통해 추정된 회귀식을 바탕으로 잔차 분석을 실시하시오 (plot포함)
#   - 적합된 값과잔차 그래프, 히스토그램, Q-Q plot를 작성(총 3개)하고 해석
set.seed(1606)
n <- nrow(sat)
idx <- 1:n
training_idx <- sample(idx, n * .60)
idx <- setdiff(idx, training_idx)
validate_idx <- sample(idx, n * .20)
test_idx <- setdiff(idx, validate_idx)
training <- sat[training_idx,]
validation <- sat[validate_idx,]
test <- sat[test_idx,]

data_lm_full <- lm(total ~ ., data=training)

summary(data_lm_full)
# Call:
#   lm(formula = total ~ ., data = training)
# 
# Residuals:
#   Min         1Q     Median         3Q        Max 
# -9.535e-13 -3.517e-14  5.590e-15  1.173e-13  1.820e-13 
# 
# Coefficients:
#               Estimate Std. Error    t value Pr(>|t|)    
# (Intercept)  2.339e-13  1.445e-12  1.620e-01    0.873    
# expend       1.506e-13  9.914e-14  1.519e+00    0.142    
# ratio        3.676e-14  2.895e-14  1.270e+00    0.217    
# salary      -4.415e-14  2.323e-14 -1.900e+00    0.070 .  
# takers       1.516e-15  4.170e-15  3.630e-01    0.720    
# verbal       1.000e+00  5.220e-15  1.916e+14   <2e-16 ***
# math         1.000e+00  4.163e-15  2.402e+14   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 2.28e-13 on 23 degrees of freedom
# Multiple R-squared:      1,	Adjusted R-squared:      1 
# F-statistic: 5.936e+29 on 6 and 23 DF,  p-value: < 2.2e-16

hist(data_lm_full$residuals)
plot(data_lm_full, which = 1)
plot(data_lm_full, which = 2)