# --------------------------------------------------------------------------- #
# 여러 모집단의 평균비교 검정 - 세 개 이상 -
# --------------------------------------------------------------------------- #

setwd("C:/Users/gksxo/Desktop/Folder/github/SCH_Multivariate_Analysis/중간고사준비")

# 데이터 읽기
ad <- read.csv("./data/age.data.csv",
                 header = T)

# 오차 제곱합 구하기
# 집단별로 데이터 나누기
y1 <- ad$age[ad$scale == "1"]
y2 <- ad$age[ad$scale == "2"]
y3 <- ad$age[ad$scale == "3"]

# 집단별 평균 구하기
y1.mean <- mean(y1)
y2.mean <- mean(y2)
y3.mean <- mean(y3)

# 집단별 편차 제곱합 구하기
y1.sse <- sum((y1 - y1.mean)^2)
y2.sse <- sum((y2 - y2.mean)^2)
y3.sse <- sum((y3 - y3.mean)^2)

# 제곱합을 모두 더해 sse에 저장하고 출력합니다.
sse <- y1.sse + y2.sse + y3.sse

# 자유도를 모두 더해 dfe에 저장하고 출력
dfe <- ((length(y1) - 1) 
        + (length(y2) - 1)
        + (length(y3) - 1))


# 처리 제곱학 구하기
# 전체 평균을 구해 변수 y에 저장합니다.
y <- mean(ad$age)

# 각 처리별로 처리의 평균과 전체 평균과의 편차 제곱합을 구하고 각 처리의 표본
# 의 개수와 곱한다.
sst.1 <- length(y1) * sum((y1.mean -  y)^2)
sst.2 <- length(y2) * sum((y2.mean -  y)^2)
sst.3 <- length(y3) * sum((y3.mean -  y)^2)

# 각 처리별로 구한 값을 모두 더해 처리제곱합을 구해 변수 sst에 저장하과 출력
# 합니다.
sst <- sst.1 + sst.2 + sst.3
dft <- length(unique(ad$scale)) - 1

# 총 편차 제곱합을 구해 tsq에 저장하고 출력합니다.
tsq <- sum((ad$age - y)^2)

# 위에서 구한 처리제곱합 (sst)와 오차제곱합 (sse)의 
# 합을 ss에 저장하고 출력합니다.
ss <- sst + sse

# 검정 통계량

# 처리 평균제곱합을 구해 변수 mst에 저장합니다.
mst <- sst / dft

# 오차 평균제곱합을 구해 변수 mse에 저장합니다.
mse <- sse / dfe

# mst를 mse로 나눈 값을 f.t에 저장하고 출력합니다.
f.t <- mst / mse

# 다음과 같이 임계값을 구할 수 있습니다.

# 유의 수준이 0.05일 때 qf() 함수를 이용해 임계값을 구한 후 변수 tol에 저장하고
# 출력합니다
alpha <- 0.05
tol <- qf(1 - alpha, 2, 147)

# 검정 통계량으로 부터 유의 확률을 구해봅시다.
p.value <- 1 - pf(f.t, 2, 147)


# ------------------------------------ #
# 위의 행위를 한번에 하는 함수 
ow <- lm(age ~ scale, 
         data = data)
# Call:
#   lm(formula = age ~ scale, data = data)
# 
# Coefficients:
#   (Intercept)        scale  
# 44.53         0.99  

anova(ow)
# Analysis of Variance Table
# 
# Response: age
#            Df Sum Sq Mean Sq F value Pr(>F)
# scale       1     98   98.01  0.4804 0.4893
# Residuals 148  30192  204.00 


