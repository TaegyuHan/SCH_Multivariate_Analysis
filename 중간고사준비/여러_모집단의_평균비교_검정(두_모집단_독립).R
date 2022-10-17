# --------------------------------------------------------------------------- #
# 여러 모집단의 평균비교 검정 - 두 모집단(독립) -
# --------------------------------------------------------------------------- #


setwd("C:/Users/gksxo/Desktop/Folder/github/SCH_Multivariate_Analysis/중간고사준비")

# 데이터 읽기
data <- read.table("./data/chapter7.txt",
                   header = T)

# --------------------------- #
# 두개의 집단 등분산 확인
# --------------------------- #

# 등분산 검정
var.test(data$weight ~ data$gender)
# F test to compare two variances
# 
# data:  data$weight by data$gender
# F = 2.1771, (검정통계량)
# num df = 17, 
# denom df = 25, 
# p-value = 0.07526 (유의확률)
# alternative hypothesis: true ratio of variances is not equal to 1
# 95 percent confidence interval:
#   0.9225552 5.5481739
# sample estimates:
#   ratio of variances 
# 2.177104 


# --------------------------- #
# 등분산 검정
# --------------------------- #
stats::t.test(
  data$weight ~ data$gender, # 데이터
  mu = 0, # 두 집단의 평균 차이
  alternative = "less", # 검정 방법
  var.equal = TRUE # 등분산 유무
)

# Two Sample t-test
# 
# data:  data$weight by data$gender
# t = -1.5229, (검정 통계량)
# df = 42, (자유도)
# p-value = 0.06764 (유의 확률)
# alternative hypothesis: true difference in means between group 1 and group 2 is less than 0
# 95 percent confidence interval:
#   -Inf 25.37242
# sample estimates:
#   mean in group 1 mean in group 2 
# 3132.444        3375.308 (평균)

