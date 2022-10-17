# ---------------------------------------------------------------- #
# 2022-09-15 (목요일) 6차 다변량 분석 강의
# 내용 : 과제!!
#
#                                                 - 김재윤 교수님 -
# ---------------------------------------------------------------- #


# ------------------------------ #
# 사용 페키지
library(tidyverse)
library(MASS)


# ---------------------------------------------------------------- #
# 단일 모집단의 평균 비교 검정
# 단일 모집단의 가설검정 (유의수준 : 0.05)
#
# 유의수준
alpha <- 0.05
# 문제 1
#   귀무가설 : 고양이의 몸무게는 2.6kg이다.
#   대립 가설: 고양이의 몸무게는 2.6kg이 아니다.
# 
# 문제 2
#   귀무가설 : 고양이의 몸무게는 2.7kg 보다 작거나 같다.
#   대립 가설: 고양이의 몸무게는 2.7kg크다.

# ------------------------------#
# MASS 패키지 cats 데이터 활용하기
MASS::cats
# Sex Bwt  Hwt
# 1     F 2.0  7.0
# 2     F 2.0  7.4
# 3     F 2.0  9.5
# 4     F 2.1  7.2
# 5     F 2.1  7.3
# 6     F 2.1  7.6
# 7     F 2.1  8.1
# ...

str(MASS::cats)
# 'data.frame':	144 obs. of  3 variables:
# $ Sex: Factor w/ 2 levels "F","M": 1 1 1 1 1 1 1 1 1 1 ...
# $ Bwt: num  2 2 2 2.1 2.1 2.1 2.1 2.1 2.1 2.1 ...
# $ Hwt: num  7 7.4 9.5 7.2 7.3 7.6 8.1 8.2 8.3 8.5 ...


# 기술 통계량
# 평균 함수
bwt.mean <- MASS::cats$Bwt %>% mean()
# [1] 2.723611

# 표준 편차
bwt.sd <- MASS::cats$Bwt %>% sd()
# [1] 0.4853066

# 길이 함수
bwt.length <- MASS::cats$Bwt %>% length()
# [1] 144

# ------------------------------#
# 문제 1
#   귀무 가설 : 고양이의 몸무게는 2.6kg이다.
#   대립 가설: 고양이의 몸무게는 2.6kg이 아니다.
h0.1 <- 2.6

# 검정 통계량
t.t1 <- (bwt.mean - h0.1) / (bwt.sd / bwt.length %>% sqrt())
# [1] 3.056487

# 임계값
c.u1 <- qt(1 - alpha, df = bwt.length - 1)
# [1] 1.655579

# 유의 확률
p.value1 <- 1 - pt(t.t1, df = bwt.length - 1)
# [1] 0.001336518


# ------------------------------#
# 문제 2
#   귀무 가설 : 고양이의 몸무게는 2.7k보다 작거나 같다.
#   대립 가설: 고양이의 몸무게는 2.7kg크다.
h0.2 <- 2.7

# 검정 통계량
t.t2 <- (bwt.mean - h0.2) / (bwt.sd / bwt.length %>% sqrt())
# [1] 0.5838233

# 임계값
c.u2 <- qt(1 - alpha, df = bwt.length - 1)
# [1] 1.655579

# 유의 확률
p.value2 <- 1 - pt(t.t2, df = bwt.length - 1)
# [1] 0.2801295

# ---------------------------------------------------------------- #
# 모집단이 두 개인 경우의 평균 비교 검정 (독립 표본 비교 분석)
# 두 모집단의 가설검정 (유의수준 : 0.05)
#
# 유의수준
alpha <- 0.05
# 문제 1
#   귀무 가설 : 암컷 고양이와 수컷 고양이의 몸무게(Bwt)는 동일하다.
#   대립 가설 : 암컷 고양이와 수컷 고양이의 몸무게(Bwt)는 동일하지 않다.
#   결과 해석 작성 : (boxplot 포함)

# 문제 2
#   귀무 가설 : 암컷 고양이와 수컷 고양이의 심장 몸무게(Hwt)는 동일하다.
#   대립 가설 : 암컷 고양이와 수컷 고양이의 심장 몸무게(Hwt)는 동일하지 않다.
#   결과 해석 작성 : (boxplot 포함)
# ------------------------------#
# MASS 패키지 cats 데이터 활용하기
MASS::cats
# Sex Bwt  Hwt
# 1     F 2.0  7.0
# 2     F 2.0  7.4
# 3     F 2.0  9.5
# 4     F 2.1  7.2
# 5     F 2.1  7.3
# 6     F 2.1  7.6
# 7     F 2.1  8.1
# ...

str(MASS::cats)
# 'data.frame':	144 obs. of  3 variables:
# $ Sex: Factor w/ 2 levels "F","M": 1 1 1 1 1 1 1 1 1 1 ...
# $ Bwt: num  2 2 2 2.1 2.1 2.1 2.1 2.1 2.1 2.1 ...
# $ Hwt: num  7 7.4 9.5 7.2 7.3 7.6 8.1 8.2 8.3 8.5 ...

# ------------------------------#
# Bwt 등분산, 이분산 확인
# 가설 수립 : 두 집단의 분산이 동일하면 그 비가 1
# 귀무 가설 : 두 집단의 분산은 서로 동일하다.
# 대립 가설 : 두 집단의 분산은 서로 동일하지 않다.

# 동일성 검정
var.test(MASS::cats$Bwt ~ MASS::cats$Sex)
# 
# F test to compare two variances
# 
# data:  MASS::cats$Bwt by MASS::cats$Sex
# F = 0.3435, num df = 46, denom df = 96, p-value = 0.0001157
# alternative hypothesis: true ratio of variances is not equal to 1
# 95 percent confidence interval:
#   0.2126277 0.5803475
# sample estimates:
# ratio of variances 
#           0.3435015 

# 판정 기각역을 이용한 판정 : 
# 유의 확률을 이용한 판정 : 유의확률은 0.001157으로 유의수준 0.05 보다 작으므로
#       귀무 가설을 기각합니다.


# ------------------------------#
# 문제 1
#   귀무 가설 : 암컷 고양이와 수컷 고양이의 몸무게(Bwt)는 동일하다.
#   대립 가설 : 암컷 고양이와 수컷 고양이의 몸무게(Bwt)는 동일하지 않다.
#   결과 해석 작성 : (boxplot 포함)

# 서로 독립인 두 모집단 : 평균 차이검정
#   - 다음으로 모집단이 다음의 가정을 만족하는지 확인합니다.
#     - '정규성'이라 하며, 이를 만족하는지 검정해야 합니다. 본 책에서는 
#         '정규성'은 만적하는 것으로 가정하겠습니다.
# 
#   - 두 집단의 분산은 서로 동일하다.
#     - '등분산성'이라 하며, R의 분산 비교 검정함수를 이용하여 분산이 서로 
#         동일한지 검정해 봅시다.

bwt.m.length <- MASS::cats$Bwt[MASS::cats$Sex == "M"] %>% length()
# [1] 97

bwt.f.length <- MASS::cats$Bwt[MASS::cats$Sex == "F"] %>% length()
# [1] 47

bwt.m.mean <- MASS::cats$Bwt[MASS::cats$Sex == "M"] %>% mean()
# [1] 2.9

bwt.f.mean <- MASS::cats$Bwt[MASS::cats$Sex == "F"] %>% mean()
# [1] 2.359574

bwt.m.sd <- MASS::cats$Bwt[MASS::cats$Sex == "M"] %>% sd()
# [1] 0.4674844

bwt.f.sd <- MASS::cats$Bwt[MASS::cats$Sex == "F"] %>% sd()
# [1] 0.2739879


t.test(MASS::cats$Bwt ~ MASS::cats$Sex,
       mu = bwt.m.mean - bwt.f.mean,
       alternative = "less",
       var.equal = FALSE)
# Welch Two Sample t-test
# 
# data:  MASS::cats$Bwt by MASS::cats$Sex
# t = -17.419, df = 136.84, p-value < 2.2e-16
# alternative hypothesis: true difference in means is less than 0.5404255
# 95 percent confidence interval:
#         -Inf -0.4376663
# sample estimates:
#   mean in group F mean in group M 
#         2.359574        2.900000 

box1Plot <- ggplot(MASS::cats, aes(x=Sex, y=Bwt)) + 
  geom_boxplot() + 
  stat_summary(fun = mean, geom = "point", shape = 20, size = 8) + 
  theme(legend.position = "none")



# ------------------------------#
# Hwt 등분산, 이분산 확인
# 가설 수립 : 두 집단의 분산이 동일하면 그 비가 1
# 귀무 가설 : 두 집단의 분산은 서로 동일하다.
# 대립 가설 : 두 집단의 분산은 서로 동일하지 않다.

# 동일성 검정
var.test(MASS::cats$Hwt ~ MASS::cats$Sex)
# F test to compare two variances
# 
# data:  MASS::cats$Hwt by MASS::cats$Sex
# F = 0.28519, num df = 46, denom df = 96, p-value = 8.159e-06
# alternative hypothesis: true ratio of variances is not equal to 1
# 95 percent confidence interval:
#   0.1765336 0.4818320
# sample estimates:
#   ratio of variances 
#            0.2851912 

# 판정 기각역을 이용한 판정 : 
# 유의 확률을 이용한 판정 : 유의확률은 8.159e-06으로 유의수준 0.05 보다 작으므로
#       귀무 가설을 기각합니다.

hwt.m.length <- MASS::cats$Hwt[MASS::cats$Sex == "M"] %>% length()
# [1] 97

hwt.f.length <- MASS::cats$Hwt[MASS::cats$Sex == "F"] %>% length()
# [1] 47

hwt.m.mean <- MASS::cats$Hwt[MASS::cats$Sex == "M"] %>% mean()
# [1] 11.32268

hwt.f.mean <- MASS::cats$Hwt[MASS::cats$Sex == "F"] %>% mean()
# [1] 9.202128

hwt.m.sd <- MASS::cats$Hwt[MASS::cats$Sex == "M"] %>% sd()
# [1] 2.542288

hwt.f.sd <- MASS::cats$Hwt[MASS::cats$Sex == "F"] %>% sd()
# [1] 1.357666


t.test(MASS::cats$Hwt ~ MASS::cats$Sex,
       mu = bwt.m.mean - bwt.f.mean,
       alternative = "less",
       var.equal = FALSE)
# Welch Two Sample t-test
# 
# data:  MASS::cats$Hwt by MASS::cats$Sex
# t = -8.1789, df = 140.61, p-value = 7.645e-14
# alternative hypothesis: true difference in means is less than 0.5404255
# 95 percent confidence interval:
#      -Inf -1.581858
# sample estimates:
#   mean in group F mean in group M 
#          9.202128       11.322680 

box2Plot <- ggplot(MASS::cats, aes(x=Sex, y=Hwt)) + 
  geom_boxplot() + 
  stat_summary(fun = mean, geom = "point", shape = 20, size = 8) + 
  theme(legend.position = "none")


# ---------------------------------------------------------------- #
# 모집단이 세 개 이상일 경우의 평균 비교 검정
# 두 모집단의 가설검정 (유의수준 : 0.05)
# Count : 살충제 살포한 후 살아남은 해충의 개수
# Spray: 6가지 살충제

# 귀무 가설 : 여섯 종류 살충제 간 살충효과의 차이는 없다(동일 하다.)
# 대립 가설 : 여섯 종류 살충제 간 살충효과의 차이는 있다(동일 하지 않다.)

str(InsectSprays)
# 'data.frame':	72 obs. of  2 variables:
# $ count: num  10 7 20 14 14 12 10 23 17 20 ...
# $ spray: Factor w/ 6 levels "A","B","C","D",..: 1 1 1 1 1 1 1 1 1 1 ...

# 변수 설정
for (spray.types in unique(InsectSprays$spray)) {
  
  # 종류별로 스프레이 별로 분할
  assign(paste0(spray.types, ".data"),
         InsectSprays$count[InsectSprays$spray == spray.types])
  
  # 개수
  # print(spray.types)
  # print(get(paste0(spray.types, ".data")) %>% length())
  # 
  # 평균
  # print(spray.types)
  # print(get(paste0(spray.types, ".data")) %>% mean())
  # 
  # # 표준 편차
  print(spray.types)
  print(get(paste0(spray.types, ".data")) %>% sd())
}


ow <- lm(count ~ spray, data=InsectSprays)
# Call:
#   lm(formula = count ~ spray, data = InsectSprays)
# 
# Coefficients:
#   (Intercept)       sprayB       sprayC       sprayD       sprayE       sprayF  
#       14.5000       0.8333     -12.4167      -9.5833     -11.0000       2.1667 
# 
anova(ow)
# Analysis of Variance Table
# 
# Response: count
# Df Sum Sq Mean Sq F value    Pr(>F)    
# spray      5 2668.8  533.77  34.702 < 2.2e-16 ***
# Residuals 66 1015.2   15.38                      
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

box3Plot <- ggplot(InsectSprays, aes(x=spray, y=count)) + 
  geom_boxplot() + 
  stat_summary(fun = mean, geom = "point", shape = 20, size = 8) + 
  theme(legend.position = "none")

# 일원 분산분석을 통해 검정한 결과,
# 검정 통계량 34.702, 유의확률2.2e-16로
# 유의 수준 0.05에서 통계적으로 유의한 차이를 보였습니다. 
# 즉, 살충제 간 살충 효과가 차이가 나는 것으로 볼 수 있습니다.


