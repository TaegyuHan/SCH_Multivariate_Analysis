# --------------------------------------------------------------------------- #
# 단일 모집단의 가설검정 - 모비율 -
# --------------------------------------------------------------------------- #



setwd("C:/Users/gksxo/Desktop/Folder/github/SCH_Multivariate_Analysis/중간고사준비")

# 데이터 읽기
data <- read.table("./data/restitution.txt",
                   header = T)

# 에러 필터링 하기
# 반발계수가 0.4134보다 작거나 0.4374보다 크면 1, 아니면 0
data <- ifelse(data$rst < 0.4134 | data$rst > 0.4374, 1, 0)

# 개수
n <- data %>% length()
# [1] 100

# 불량품 개수 
n_error <- sum(data)
# [1] 11

# 불량품 비율 (표본비율)
sp <- n_error / n
# [1] 0.11

# 모비율
hp <- 0.1
# [1] 0.1

# 검정통계량 (Z 분포)
Z_value <- ((sp - hp) / sqrt((hp*(1 - hp)) / n))
# [1] 1.644854

# 유의수준
alpah <- 0.05

# 임계값(Z 분포)
c_u <- qnorm(1 - alpha)
# [1] 0.3746353


# ------------------------------------ #
# 위의 행위를 한번에 하는 함수 prop.test

# 양쪽 검정 : "two.sided"
# (왼쪽) 한쪽검정 : "less"
# (오른쪽) 한쪽검정 : "greater"

?stats::prop.test

stats::prop.test(
  x = n_error, 
  n = n, 
  p = 0.1, 
  alternative = "greater",
  conf.level = 0.95, 
  correct = FALSE
)
# 1-sample proportions test without continuity correction
# 
# data:  n_error out of n, null probability 0.1
# X-squared = 0.11111, (검정통계량)
# df = 1, (자유도)
# p-value = 0.3694 (유의확률)
# alternative hypothesis: true p is greater than 0.1
# 95 percent confidence interval:
#   0.0684615 1.0000000
# sample estimates:
#   p 
# 0.11 (표본비율) 