# --------------------------------------------------------------------------- #
# 여러 모집단의 평균비교 검정 - 두 모집단(대응) -
# --------------------------------------------------------------------------- #


setwd("C:/Users/gksxo/Desktop/Folder/github/SCH_Multivariate_Analysis/중간고사준비")

# 데이터 읽기
data <- read.csv("./data/01.anorexia.csv",
                   header = T)


# 표본 개수
n <- length(data$Prior - data$Post)
# [1] 17

# 표본 평균
X <- mean(data$Prior - data$Post)
# [1] -7.264706

# 표본 편차
s <- sd(data)
# [1] 7.157421

t_value <- X / (s / sqrt(n))
# [1] -4.184908


# T 검정
stats::t.test(data$Prior, # 데이터
              data$Post, # 데이터
              paired=TRUE, # 데이터
              alternative="less")
# Paired t-test
# 
# data:  data$Prior and data$Post
# t = -4.1849, (검정 통계량)
# df = 16, (자유도)
# p-value = 0.0003501 (유의수준)
# alternative hypothesis: true difference in means is less than 0
# 95 percent confidence interval:
#   -Inf -4.233975
# sample estimates:
#   mean of the differences 
# -7.264706 (평균)


