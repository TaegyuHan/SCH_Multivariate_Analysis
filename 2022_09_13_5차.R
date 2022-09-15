# ---------------------------------------------------------------- #
# 2022-09-13 (화요일) 5차 다변량 분석 강의
# 내용 : 두 모집단의 평균 비교 분석
# 
#                                                 - 김재윤 교수님 -  
# ---------------------------------------------------------------- #

# 독립 표본 : 서로 독립인 두 집단.
# 쌍체 표본 : 서로 대응인 두 집단.

# 두 모집단의 평균 비교 분석
#  - 독립 표본인 경우
#     - 분산의 동일성 검정 수행 F-검정 : 분산에 대한 두 집단 (중간 과정 실시해야한다.)
#       - t-검정 : 분산에 대한 두 집단
#         - t-검정 : 등분산 가정 두 집단
#         - t-검정 : 이분산 가정 두 집단

#  - 쌍체표본인 경우
#     - t-검정 : 쌍체비교

# 아노바 분석 : 3개 이상의 집단 분산 분석


library(tidyverse)

# ---------------------------------------------------------------- #
# 독립 표본 비교 분석
# ----------------------------
# 가설 수립 : 두 집단의 분산이 동일하면 그 비가 1이다.
# 귀무 가설 : 두 집단의 분산은 서로 동일하다.
# 대립 가설 : 두 집단의 분산은 동일하지 않다.

# 데이터 읽기
dataPath <- "C:/Users/student/Desktop/다변량분석/data/4차/실습데이터/chapter7.txt"
data1 <- read.table(file = dataPath,
                   header = T)

str(data)
# 'data.frame':	44 obs. of  2 variables:
# $ gender: int  1 1 2 2 2 1 1 2 2 2 ...
# $ weight: int  3837 3334 3554 3838 3625 2208 1745 2846 3166 3520 ...

data1$gender <- as.factor(data1$gender)

box1Plot <- ggplot(data1, aes(x=gender, y=weight)) + 
  geom_boxplot() + 
  stat_summary(fun = mean, geom = "point", shape = 20, size = 8) + 
  theme(legend.position = "none")


var.test(data1$weight ~ data1$gender)
# data$weight : 수치형
# data$gender : 범주형

# F test to compare two variances
# 
# data:  data$weight by data$gender
# F = 2.1771, num df = 17, denom df = 25, p-value = 0.07526
# alternative hypothesis: true ratio of variances is not equal to 1
# 95 percent confidence interval:
#   0.9225552 5.5481739
# sample estimates:
#   ratio of variances 
# 2.177104 


# 판정
# 기각역을 이용한 판정 : 검정통계량이 채택역 구간인 
#       (0.39, 2.36) 사이에 있어 영가설을 채택합니다.

# 유의 확률을 이용한 판정 : 유의 확률은 
#       0.07526으로 유의수준 0.05 보다 크므로 영 가설을 채택합니다.


t.test(data1$weight ~ data1$gender,
       mu = 0,
       alternative = "less",
       var.equal = TRUE)
# Two Sample t-test
# 
# data:  data$weight by data$gender
# t = -1.5229, df = 42, p-value = 0.06764
# alternative hypothesis: true difference in means is less than 0
# 95 percent confidence interval:
#   -Inf 25.37242
# sample estimates:
#   mean in group 1 mean in group 2 
# 3132.444        3375.308 


# ---------------------------------------------------------------- #
# 쌍체 표본 비교 분석
# ----------------------------
# 귀무 가설 : 신경성 식욕부진증 치료법은 효과가 없다.
# 대립 가설 : 신경성 식욕부진증 치료법은 효과가 있다.
dataPath <- "C:/Users/student/Desktop/다변량분석/data/4차/실습데이터/01.anorexia.csv"
data2 <- read.csv(file = dataPath,
                 header = TRUE)

df <- data.frame(
  value_col = c(data2$Prior, data2$Post),
  type_col = as.factor(c(
    rep("Prior", times = length(data2$Prior)),
    rep("Post", times = length(data2$Post))
  ))
)

box2Plot <-ggplot(df, aes(x=type_col, y=value_col)) + 
  geom_boxplot() + 
  stat_summary(fun = mean, geom = "point", shape = 20, size = 8) + 
  theme(legend.position = "none")


# 검정 통계량 계산
n <- length(data2$Prior - data2$Post)
m <- mean(data2$Prior - data2$Post)
s <- sd(data2$Prior - data2$Post)
t_t <- (m / (s / sqrt(n)))
# 검정 통계량 약 : -4.184908

# t.test를 이용한 검정
t.test(data2$Prior, data2$Post,
       paired = TRUE,
       alternative = "less")
# Paired t-test
# 
# data:  data$Prior and data$Post
# t = -4.1849, df = 16, p-value = 0.0003501
# alternative hypothesis: true difference in means is less than 0
# 95 percent confidence interval:
#   -Inf -4.233975
# sample estimates:
#   mean of the differences 
# -7.264706 

# 판정
# - 기각역을 이용한 판정 : 
#         검정 통계량 -4.185는 기각역에 속하므로 귀무가설을 기각합니다.
# - 유의확률을 이용한 판정 : 
#         검정 통계량으로부터 유의확률을 0.00035로 유의수준보다 작아 
#         귀무가설을 기가합니다.




