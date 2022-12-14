# ---------------------------------------------------------------- #
# 2022-09-22 (목요일) 4주 8차 다변량 분석 강의
# 내용 : 상관 분석 및 회귀 분석
# 
#                                                 - 김재윤 교수님 -  
# ---------------------------------------------------------------- #

# ---------------------------------------------------------------- #
# 범주 - 범주 > 독립성
# 수치 - 수치 > 상관 분석
# 수치 - 범주 > ???
# ---------------------------------------------------------------- #

# ---------------------------------------------------------------- #
# 01. 상관 계수 
#     : 두 변수 간 관계의 정도
# 
# -1 ~ 1의 값을 가진다.
# 의미가 있는 데이터 이지만 0의 결과가 나올 수 있다.
# 결국 그래프를 그려서 확인하는 것이 중요 하다.
# ---------------------------------------------------------------- #


# ---------------------------------------------------------------- #
# 공분산
# 두 변수 사이 상관의 정도를 나타낸다.
# C(X, Y) < 공분산 이다.
# C(X, X) < 분산 이다.
# 
# 공분산의 문제점
# 어느 정도 차이가 나는지 비교를 못한다.
# 공분산의 가장 큰 단점은 측정단위에 따라 그 값이 달라진다는 것!
# 자동차 kg 와 자동차 가격의 비교
# > 따라서 표준화 시켜서 비교한다.
# 
# 그래프의 기울기 값이 상관 계수의 값이 아니다.
# ---------------------------------------------------------------- #


# ---------------------------------------------------------------- #
# 아버지와 아들의 키 자료로 부터 
# 아버지와 아들의 키의 공분산과 상관관계 수를 구해봅시다.


hf <- read.csv("C:/Users/student/Desktop/다변량분석/data/8차/Galton/Galton.csv")

str(hf)
# 'data.frame':	898 obs. of  6 variables:
# $ Family: chr  "1" "1" "1" "1" ...
# $ Father: num  78.5 78.5 78.5 78.5 75.5 75.5 75.5 75.5 75 75 ...
# $ Mother: num  67 67 67 67 66.5 66.5 66.5 66.5 64 64 ...
# $ Gender: chr  "M" "F" "F" "F" ...
# $ Height: num  73.2 69.2 69 69 73.5 72.5 65.5 65.5 71 68 ...
# $ Kids  : int  4 4 4 4 4 4 4 4 2 2 ...

# 팩터형으로 변형하기
hf$Gender <- factor(hf$Gender, levels = c("M", "F"))

# Father키와 자신의 키 추출
hf.son <- subset(hf, Gender == "M")
hf.son <- hf.son[c("Father", "Height")]

f.mean <- mean(hf.son$Father)
s.mean <- mean(hf.son$Height)
# > f.mean
# [1] 69.16817
# > s.mean
# [1] 69.22882

cov.num <- sum((hf.son$Father - f.mean) * (hf.son$Height - s.mean))
# [1] 1098.956
cov.xy <- cov.num / (nrow(hf.son) - 1)
# [1] 2368441

cov(hf.son$Father, hf.son$Height)
# [1] 2368441

r.xy <- cov.xy / (sd(hf.son$Father) * sd(hf.son$Height))
# [1] 0.3913174

cor(hf.son$Father, hf.son$Height)
# [1] 0.3913174

# 상관 분석 그래프
# Creating the plot
x <- hf.son$Father
y <- hf.son$Height
plot(x, y, pch = 19, col = "lightblue")

# Regression line
abline(lm(y ~ x), col = "red", lwd = 3)

# Pearson correlation
text(paste("Correlation:", round(cor(x, y), 2)), x = 25, y = 95)

# ---------------------------------------------------------------- #
