# --------------------------------------------------------------------------- #
# 선형회귀분석 - 상관계수 -
# --------------------------------------------------------------------------- #

setwd("C:/Users/gksxo/Desktop/Folder/github/SCH_Multivariate_Analysis/중간고사준비")

# 데이터 읽기
data <- read.csv("./data/Galton.csv",
                   header = T)


# 상관계수 확인하기
cor(data$Father, data$Height)

# install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)

# 수치형만 가능함
chart.Correlation(data, 
                  histogram = TRUE, 
                  method = "pearson")
