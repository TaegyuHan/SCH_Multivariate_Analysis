# ------------------------------------------------------------------- # 
#  중간고사 
# ------------------------------------------------------------------- # 


# 아래의 데이터를 생성한 후 다음을 분석하시오
library(tidyverse)

# ---------------------------------- #
# 1번 문제

x <- runif(1000, 0, 10)

x %>% length()
# [1] 1000

y <- 2 + 3*x + rnorm(1000, mean = 0, sd = 3)

# 문제 1
# y는 종속변수, x는 독립 변수

# 시각화
plot(x, y)

# 회귀선
abline(lm(y ~ x), col = "red", lwd = 3)

# 상관계수
cor(x, y)


# 회귀식
lm_out <- lm(y ~ x)
# Call:
#   lm(formula = y ~ x)
# 
# Coefficients:
#   (Intercept)            x  
# 1.66         3.05 
# y = 1.66 + 3.05x1

summary(lm_out)
# Call:
#   lm(formula = y ~ x)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -8.9365 -1.9537  0.0499  1.8643  8.8704 
# 
# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)  1.65961    0.18211   9.113   <2e-16 ***
#   x            3.05047    0.03204  95.217   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 2.946 on 998 degrees of freedom
# Multiple R-squared:  0.9008,	Adjusted R-squared:  0.9007 
# F-statistic:  9066 on 1 and 998 DF,  p-value: < 2.2e-16


par(mfrow = c(1, 3))

# Residuals VS Fitted
plot(lm_out, which = 1)

# Normal Q-Q
plot(lm_out, which = 2)

# Cook's distance
hist(x, y)

pred_1 <- predict(lm_out, newdata = data.frame(x = c(0, 10, 100)))


# ---------------------------------- #
# 2번 문제
data("airquality")

data2 <- airquality

data2 <- data2 %>% na.omit()

data2 %>% str()
# 종속변수 : Ozone

# 'data.frame':	111 obs. of  6 variables:
# $ Ozone  : int  41 36 12 18 23 19 8 16 11 14 ...
# $ Solar.R: int  190 118 149 313 299 99 19 256 290 274 ...
# $ Wind   : num  7.4 8 12.6 11.5 8.6 13.8 20.1 9.7 9.2 10.9 ...
# $ Temp   : int  67 72 74 62 65 59 61 69 66 68 ...
# $ Month  : int  5 5 5 5 5 5 5 5 5 5 ...
# $ Day    : int  1 2 3 4 7 8 9 12 13 14 ...
# - attr(*, "na.action")= 'omit' Named int [1:42] 5 6 10 11 25 26 27 32 33 34 ...
# ..- attr(*, "names")= chr [1:42] "5" "6" "10" "11" ...


ow <- lm(Ozone ~ ., data = data2)
# Call:
#   lm(formula = Ozone ~ ., data = data2)
# 
# Coefficients:
#   (Intercept)      Solar.R         Wind         Temp        Month  
# -64.11632      0.05027     -3.31844      1.89579     -3.03996  
# Day  
# 0.27388 

summary(ow)
# Call:
#   lm(formula = Ozone ~ ., data = data2)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -37.014 -12.284  -3.302   8.454  95.348 
# 
# (회귀 계수)
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
#  (Intercept) -64.11632   23.48249  -2.730  0.00742 ** 
#   Solar.R       0.05027    0.02342   2.147  0.03411 *  
#   Wind         -3.31844    0.64451  -5.149 1.23e-06 ***
#   Temp          1.89579    0.27389   6.922 3.66e-10 ***
#   Month        -3.03996    1.51346  -2.009  0.04714 *  
#   Day           0.27388    0.22967   1.192  0.23576    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# (모형 적합성)
# Residual standard error: 20.86 on 105 degrees of freedom
# Multiple R-squared:  0.6249,	Adjusted R-squared:  0.6071 
# F-statistic: 34.99 on 5 and 105 DF,  p-value: < 2.2e-16

anova(ow)

# 문제에서 유의하지 않은 변수들을 제거하고 회귀모형 구축하기
# Solar.R, Wind, Temp
change_lm <- lm(Ozone ~ Solar.R + Wind + Temp, data = data2)
# Call:
#   lm(formula = Ozone ~ Solar.R + Wind + Temp, data = data2)
# 
# Coefficients:
#   (Intercept)      Solar.R         Wind         Temp  
# -64.34208      0.05982     -3.33359      1.65209  

# y = 0.05982x1 -3.33359x2 + 1.65209x3 -64.34208

summary(change_lm)
# Call:
#   lm(formula = Ozone ~ Solar.R + Wind + Temp, data = data2)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -40.485 -14.219  -3.551  10.097  95.619 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -64.34208   23.05472  -2.791  0.00623 ** 
#   Solar.R       0.05982    0.02319   2.580  0.01124 *  
#   Wind         -3.33359    0.65441  -5.094 1.52e-06 ***
#   Temp          1.65209    0.25353   6.516 2.42e-09 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 21.18 on 107 degrees of freedom
# Multiple R-squared:  0.6059, (결정 계수)
# Adjusted R-squared:  0.5948 
# F-statistic: 54.83 on 3 and 107 DF,  p-value: < 2.2e-16


# ---------------------------------- #
# 3번 문제
library(ggplot2)
na_not_diamonds <- diamonds %>% na.omit()

na_not_diamonds %>% str()

# tibble [53,940 x 10] (S3: tbl_df/tbl/data.frame)
# $ carat  : num [1:53940] 0.23 0.21 0.23 0.29 0.31 0.24 0.24 0.26 0.22 0.23 ...
# $ cut    : Ord.factor w/ 5 levels "Fair"<"Good"<..: 5 4 2 4 2 3 3 3 1 3 ...
# $ color  : Ord.factor w/ 7 levels "D"<"E"<"F"<"G"<..: 2 2 2 6 7 7 6 5 2 5 ...
# $ clarity: Ord.factor w/ 8 levels "I1"<"SI2"<"SI1"<..: 2 3 5 4 2 6 7 3 4 5 ...
# $ depth  : num [1:53940] 61.5 59.8 56.9 62.4 63.3 62.8 62.3 61.9 65.1 59.4 ...
# $ table  : num [1:53940] 55 61 65 58 58 57 57 55 61 61 ...
# $ price  : int [1:53940] 326 326 327 334 335 336 336 337 337 338 ...
# $ x      : num [1:53940] 3.95 3.89 4.05 4.2 4.34 3.94 3.95 4.07 3.87 4 ...
# $ y      : num [1:53940] 3.98 3.84 4.07 4.23 4.35 3.96 3.98 4.11 3.78 4.05 ...
# $ z      : num [1:53940] 2.43 2.31 2.31 2.63 2.75 2.48 2.47 2.53 2.49 2.39 ...
# 둘다 범주형

chisq.test(na_not_diamonds$cut, na_not_diamonds$color)
# Pearson's Chi-squared test
# 
# data:  na_not_diamonds$cut and na_not_diamonds$color
# X-squared = 310.32, (검정 통계량)
# df = 24, (자유도)
# p-value < 2.2e-16 (유의 확률)

par(mfrow = c(1, 1))
mosaicplot(cut ~ color, data = na_not_diamonds)


na_not_diamonds %>% 
  group_by(cut) %>% 
  summarise(mean(price))
# cut       `mean(price)`
# <ord>             <dbl>
# 1 Fair              4359.
# 2 Good              3929.
# 3 Very Good         3982.
# 4 Premium           4584.
# 5 Ideal             3458.



temp_color <- ifelse(na_not_diamonds$cut=="Fair", 4359.,
         ifelse(na_not_diamonds$cut=="Good", 3929.,
                ifelse(na_not_diamonds$cut=="Very Good", 3982.,
                       ifelse(na_not_diamonds$cut=="Premium", 4584., 3458.)))) %>% as.factor()

chisq.test(na_not_diamonds$cut, temp_color)
# Pearson's Chi-squared test
# 
# data:  na_not_diamonds$cut and temp_color
# X-squared = 215760, (검정통계량)
# df = 16, (자유도)
# p-value < 2.2e-16 (유의확률)

boxplot()






