# --------------------------------------------------------------------------- #
# 선형회귀분석 - 회귀분석 -
# --------------------------------------------------------------------------- #

setwd("C:/Users/gksxo/Desktop/Folder/github/SCH_Multivariate_Analysis/중간고사준비")

# 데이터 읽기
data <- read.csv("./data/Galton.csv",
                 header = T)

# R 함수를 이용한 선형회귀

lm_out <- lm(Height ~ Father, data = data)
# Call:
#   lm(formula = Height ~ Father, data = data)
# 
# Coefficients:
#   (Intercept)       Father  
#       39.1104       0.3994 > 으로 식 도출 y = 39.1104 + 0.3994(x1)


# 회귀모형의 유의성 검정
anova(lm_out)
# Analysis of Variance Table
# 
# Response: Height
#            Df  Sum Sq Mean Sq F value    Pr(>F)    
# Father      1   873.1  873.08  73.508 < 2.2e-16 ***
# Residuals 896 10642.0   11.88                      
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


summary(lm_out)
# Call:
#   lm(formula = Height ~ Father, data = data)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -10.2683  -2.6689  -0.2092   2.6342  11.9329 
# 
# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
#   (Intercept) 39.11039    3.22706  12.120   <2e-16 *** # 식 에러
#   Father       0.39938    0.04658   8.574   <2e-16 *** # 변수 계수
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 3.446 on 896 degrees of freedom
# Multiple R-squared:  0.07582,	(결정계수)
# Adjusted R-squared:  0.07479  (수정결정계수)
# F-statistic: 73.51 (검정통계량)
# on 1 and 
# 896 DF,  (자유도)
# p-value: < 2.2e-16 (유의확률)


# 시각화
# 독립성
plot(lm_out, which = 1)

# 등분산성
plot(lm_out, which = 3)

# 정규성
plot(lm_out, which = 2)

# (잔차)히스토그램
hist(lm_out$residuals)

