# ---------------------------------------------------------------- #
# 2022-10-04 (목요일) 6주 11차 다변량 분석 강의
# 내용 : 회귀 분석
#                                                 - 김재윤 교수님 -  
# ---------------------------------------------------------------- #


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

# weight에 대한 height의 산점도를 그리시오.
plot(women$weight, women$height)

# 단순선형회귀직선을 구하여 산점도에 함께 나타내시오.
lines(women.lm$fitted.values, women$height)

# 모형 적합성에 대해 검정하시오.
# 귀무가설 : 
# 대립가설 : 

# 회귀계수가 유의한지 검정하시오.
# 귀무가설 : 독립변수는 종속변수와 선형관계를 갖지 않는다.
# 대립가설 : 독립변수는 종속변수와 선형관계를 갖는다.

# 결정계수를 구하고 해석하시오.

# 잔차에 대해 독립성, 등분산성, 정규성을 만족하는 잔차그림
# Q - Q 그림을 그리고 설명 하시오.
plot(women.lm, which = 1)
plot(women.lm, which = 2, pch = 16)


# -------------- #
# number2
data(airquality)
str(airquality[,1:4])
# 'data.frame':	153 obs. of  4 variables:
# $ Ozone  : int  41 36 12 18 NA 28 23 19 8 NA ...
# $ Solar.R: int  190 118 149 313 NA NA 299 99 19 194 ...
# $ Wind   : num  7.4 8 12.6 11.5 14.3 14.9 8.6 13.8 20.1 8.6 ...
# $ Temp   : int  67 72 74 62 56 66 65 59 61 69 ...

# Ozone 종속변수이며, Solar.R(일조량), Wind(풍속), Temp(기온)
# (식)> Ozone ~ Solar.R + Wind + Temp

# 회귀 모형 식
# y = b0x0 + b0x0 + b0x0 + E

# 각각의 상관분석을 실시 해보기


# - 각 변수들간의 상관분석 및 산점도 그리기

# - 3개의 독립변수를 활용하여 다중회귀분석을 실시

# - 모형 적합성 및 회귀계수의 유의성 검정하기
# 회귀모형의 유의성 검정

# 회귀계수의 유의성 검정


# - 잔차분석(그래프 그리기: 
#         - (잔차)히스토그램, 
#         - 적합값과 잔차 산점도, 
#         - Q-Q plot)












