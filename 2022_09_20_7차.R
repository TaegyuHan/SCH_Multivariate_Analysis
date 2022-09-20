# ---------------------------------------------------------------- #
# 2022-09-20 (화요일) 7차 다변량 분석 강의
# 내용 : 범주형 데이터
# 
#                                                 - 김재윤 교수님 -  
# ---------------------------------------------------------------- #


# ---------------------------------------------------------------- #
# 적합도 검정
# 동질성 검정과 < 이것은 하지 않는다.
# 독립성 검정
# ---------------------------------------------------------------- #

# 적합도 검정 > 변수가 1개 이다.
# 독립성 검정 > 변수가 2개 이다.
# 두 변수가 독립적인이 독립적인지 아닌지.

# ---------------------------------------------------------------- #
# 적합도 검정 (변수가 1개 일때)
# 이미 알려져 있는 규칙이 실제 실험한 데이터와 동일한지 검정하는 방법

# 기대 도수(E)
# - 각 범주의 영가설(귀무가설) 하에서의 비율을 곱해 계산된 수를 
#       기대도수라고 합니다.

# 관찰도수(O)
# - 앞서 실험을 통해 관찰한 각 형질의 개수와 같이 각 범주별로 관찰한
#       개수를 관찰도수라고 합니다.

1 - pchisq(0.470024, df = 3)
# [1] 0.9254259

x <- c(315, 101, 108, 32)
chisq.test(x, p=c(9, 3, 3, 1)/16)
# 
# Chi-squared test for given probabilities
# 
# data:  x
# X-squared = 0.47002, df = 3, p-value = 0.9254

# 비율 / 실제 측정값
# A : 34%, 4
# B : 27%, 8
# O : 28%, 12
# AB : 11%, 8

# ---------------------------------------------------------------- #
# 동질성 검정과 독립성 검정 (변수가 2개 범주형)
# 상관분석 : (변수 2개 수치형)

data("UCBAdmissions")
UCBAdmissions
# , , Dept = A
# 
# Gender
# Admit      Male Female
# Admitted  512     89
# Rejected  313     19
# 
# , , Dept = B
# 
# Gender
# Admit      Male Female
# Admitted  353     17
# Rejected  207      8
# 
# , , Dept = C
# 
# Gender
# Admit      Male Female
# Admitted  120    202
# Rejected  205    391
# 
# , , Dept = D
# 
# Gender
# Admit      Male Female
# Admitted  138    131
# Rejected  279    244
# 
# , , Dept = E
# 
# Gender
# Admit      Male Female
# Admitted   53     94
# Rejected  138    299
# 
# , , Dept = F
# 
# Gender
# Admit      Male Female
# Admitted   22     24
# Rejected  351    317

ucba.tab <- apply(UCBAdmissions,
                  c(1, 2),
                  sum)
# Gender
# Admit      Male Female
# Admitted   1198    557
# Rejected   1493   1278

# 독립성 검정
a.n <- margin.table(ucba.tab, margin = 1)
# Admit
# Admitted Rejected 
# 1755     2771 
g.n <- margin.table(ucba.tab, margin = 2)
# Gender
# Male Female 
# 2691   1835 

a.p <- a.n / margin.table(ucba.tab)
# Admit
# Admitted  Rejected 
# 0.3877596 0.6122404

g.p <- g.n / margin.table(ucba.tab)
# Gender
#      Male    Female 
# 0.5945647 0.4054353 
expected <- margin.table(ucba.tab) * (a.p %*% t(g.p))
addmargins(expected)


## chi-square statistic
o.e <- (ucba.tab - expected)^2 / expected
addmargins(o.e)
# Gender
# Admit          Male    Female  Sum
# Admitted 1043.461  711.5389 1755
# Rejected 1647.539 1123.4611 2771
# Sum      2691.000 1835.0000 4526

chisq.t <- sum(o.e)
chisq.t
# [1] 92.20528

qchisq(0.95, df = 1)
# [1] 3.841459

1 - pchisq(112.250, df = 1)
# [1] 0

1 - pchisq(chisq.t, df = 1)
# [1] 0

chisq.test(ucba.tab)
# 
# Pearson's Chi-squared test with Yates' continuity correction
# 
# data:  ucba.tab
# X-squared = 91.61, df = 1, p-value < 2.2e-16

## continuity correction
o.e2 <- (abs(ucba.tab - expected)-0.5)^2 / expected
sum(o.e2)
# Gender
# Admit          Male   Female
#   Admitted 22.73969 33.34741
#   Rejected 14.40207 21.12043

chisq.test(ucba.tab, correct=FALSE)
# 
# Pearson's Chi-squared test
# 
# data:  ucba.tab
# X-squared = 92.205, df = 1, p-value < 2.2e-16

# ---------------------------------------------------------------- #
# 종속 vs 독립변수 (독립 x)
# 독립 vs 독립변수 (독립 o) > 다중공선성 (PCA), 변수 축약
# 성별, 학력, 연력대, 자가유무

