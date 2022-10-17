# --------------------------------------------------------------------------- #
# 범주형 자료 분석 - 동질성 검정과 독립성 검정 -
# --------------------------------------------------------------------------- #



# 데이터
data("UCBAdmissions")
UCBAdmissions

# 데이터 전처리
ucba.tab <- apply(UCBAdmissions,
                  c(1, 2),
                  sum)



# ------------------------------- #
# 동질성과 검정과 독립성 검정
chisq.test(ucba.tab)
# Pearson's Chi-squared test with Yates' continuity correction
# 
# data: ucba.tab
# X-squared = 91.61, 
# df = 1, 
# p-value < 2.2e-16


# ------------------------------- #
# 독립성 검정 연속성 수정 X
chisq.test(ucba.tab, correct=FALSE)
# 
# Pearson's Chi-squared test
# 
# data:  ucba.tab
# X-squared = 92.205, 
# df = 1, 
# p-value < 2.2e-16




