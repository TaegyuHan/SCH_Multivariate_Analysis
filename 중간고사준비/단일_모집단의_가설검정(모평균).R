# --------------------------------------------------------------------------- #
# 단일 모집단의 가설검정 - 모평균 -
# --------------------------------------------------------------------------- #


library(tidyverse)


# 데이터 읽기
data <- data.frame(
  weight = c(3837, 3334, 2208, 1745, 2576, 3208, 
    3746, 3523, 3430, 3480, 3116, 3428, 
    2184,	2383, 3500, 3866, 3542, 3278)
)


# 데이터 확인하기
data %>% str()
# 'data.frame':	18 obs. of  1 variable:
# $ weight: num  3837 3334 2208 1745 2576 ...


# 표본 평균
X <- data$weight %>% mean()
# [1] 3132.444

# 표본 편차
s <- data$weight %>% sd()
# [1] 631.5825

# 개수
n <- data$weight %>% length()
# [1] 18

# 모 평균 : 영가설(귀무가설)
H_0 <- 2800

# t검정 식 (검정 통계량) (t-분포)
t.value <- (X - H_0) / (s / sqrt(n))
# [1] 2.233188

# 유의 수준
alpha <- 0.05

# df : 자유도
# C_u : 기각역(임계값)
C_u <- qt(1 - alpha, df = n - 1)
# [1] 1.739607

# 유의확률 (p-value)
p_value <- 1 - pt(t.value, df = n - 1)


# ------------------------------------ #
# 위의 행위를 한번에 하는 함수 t.test

# 양쪽 검정 : "two.sided"
# (왼쪽) 한쪽검정 : "less"
# (오른쪽) 한쪽검정 : "greater"

stats::t.test(x = data$weight, # 데이터
              mu = 2800, # 모형균
              # conf.level = 0.90 # < 신뢰수준 변경하는 코드
              alternative = "greater") # 검정 방법
# One Sample t-test
# 
# data:  data$weight
# t = 2.2332, (t 검정 통계량)
# df = 17, (자유도) 
# p-value = 0.01963 (유의확률)
# 
# (검정 위치)
# alternative hypothesis: true mean is greater than 2800
# 95 percent confidence interval:
#   2873.477      Inf
# sample estimates:
#   mean of x 
# 3132.444 (평균)