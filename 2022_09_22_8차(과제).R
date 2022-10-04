# ---------------------------------------------------------------- #
# 2022-09-22 (목요일) 8차 다변량 분석 강의
# 내용 : 범주형 데이터 분석 과제
# 
#                                                 - 김재윤 교수님 -  
# ---------------------------------------------------------------- #

# ---------------------------------------------------------------- #
# 사용 라이브러리
library(tidyverse)
library(randomForest)

# ---------------------------------------------------------------- #
# 사용 데이터
# 확인하기

str(MASS::survey)
# 'data.frame':	237 obs. of  12 variables:
# $ Sex   : Factor w/ 2 levels "Female","Male": 1 2 2 2 2 1 2 1 2 2 ...
#   > 학생 성별 (범주형)

# $ Wr.Hnd: num  18.5 19.5 18 18.8 20 18 17.7 17 20 18.5 ...
#   > 주 손의 한 뼘의 길이 (수치형)

# $ NW.Hnd: num  18 20.5 13.3 18.9 20 17.7 17.7 17.3 19.5 18.5 ...
#   > 주 손이 아닌 한뼘의 길이 (수치형)

# $ W.Hnd : Factor w/ 2 levels "Left","Right": 2 1 2 2 2 2 2 2 2 2 ...
#   > 주손 위치 (범주형)
#     1 : "Left"
#     2 : "Right"

# $ Fold  : Factor w/ 3 levels "L on R","Neither",..: 3 3 1 3 2 1 1 3 3 3 ...
#   > 팔짱 꼈을 때 위에 올라가는 손 (범주형)
#     1 : "L"
#     2 : "R"
#     3 : "Neither"

# $ Pulse : int  92 104 87 NA 35 64 83 74 72 90 ...
#   > 맥박 (수치형)

# $ Clap  : Factor w/ 3 levels "Left","Neither",..: 1 1 2 2 3 3 3 3 3 3 ...
#   > 박수를 칠 때 위에 있는 손 (범주형)
#     1 : "L"
#     2 : "R"
#     3 : "Neither"

# $ Exer  : Factor w/ 3 levels "Freq","None",..: 3 2 2 2 3 3 1 1 3 3 ...
#   > 일상 운동 횟수 (범주형)
#     1 : "Freq"
#     2 : "Some"
#     3 : "None"

# $ Smoke : Factor w/ 4 levels "Heavy","Never",..: 2 4 3 2 2 2 2 2 2 2 ...
#   > 흡연 강도 (범주형)
#     1 : "Heavy"
#     2 : "Regul"
#     3 : "Occas"
#     4 : "Never"

# $ Height: num  173 178 NA 160 165 ...
#   > 키 (수치형)

# $ M.I   : Factor w/ 2 levels "Imperial","Metric": 2 1 NA 2 2 1 1 2 2 2 ...
#   키 측정 단위>  (범주형)
#     1 : "Metric" (centimetres/metres)
#     2 : "Imperial" (feet/inches)

# $ Age   : num  18.2 17.6 16.9 20.3 23.7 ...
#   나이>  (수치형)
# ---------------------------------------------------------------- #

# ---------------------------------------------------------------- #
# 과제 1
# MASS 패키지의 내장된 데이터 활용 (survey 데이터)

# (1) 
# 자유롭게 변수들을 선택하여 
#   - 독립 표본 t-검정
#   - 분산분석(ANOVA)
# 을 각각 실시 (각 2개씩)
# 
# - 귀무가설과 대립가설도 자유롭게 설정
# - 유의수준 (0.05)를 기준으로 통계적 의사결정 (검정통계량과 p-value를 확인)
# - botplot 그리기

# (2)
# 자유롭게 변수들을 선택하여 적합성, 독립성 검정을 각각 실시 (각 2개씩)
#   - 귀무가설과 대립가설 설정
#   - 분석할 변수들을 바탕으로 교차 테이블(분할표) 작성하기
#   - 유의수준 (0.05)를 기준으로 통계적 의사결정 (검정통계량과 p-value를 확인)

# (3)
# 수치형-수치형 변수
# 상관 계수 및 산점도 그리기
# ---------------------------------------------------------------- #


# ---------------------------------------------------------------- #
# 자유롭게 변수들을 선택하여 (독립 표본 t-검정)
# (각 2개씩)
# 
# - 귀무가설과 대립가설도 자유롭게 설정
# - 유의수준 (0.05)를 기준으로 통계적 의사결정 (검정통계량과 p-value를 확인)
# - botplot 그리기

# ------------------------------------------- #
# 1)
# 귀무가설 : 남자와 여자의 Wr.Hnd 주손(주로 사용하는 손)의 한뼘 길이는 동일하지 않다.
# 대립가설 : 남자와 여자의 Wr.Hnd 주손의 한뼘 길이는 동일하다.

# ------------------------------#
# Wr.Hnd 등분산, 이분산 확인
# 가설 수립 : 두 집단의 분산이 동일하면 그 비가 1
# 귀무 가설 : 두 집단의 분산은 서로 동일하다.
# 대립 가설 : 두 집단의 분산은 서로 동일하지 않다.

# 동일성 검정
var.test(MASS::survey$Wr.Hnd ~ MASS::survey$Sex)
# F test to compare two variances
# 
# data:  MASS::survey$Wr.Hnd by MASS::survey$Sex
# F = 0.56395, num df = 117, denom df = 116, p-value = 0.002174
# alternative hypothesis: true ratio of variances is not equal to 1
# 95 percent confidence interval:
#   0.3914479 0.8122226
# sample estimates:
#   ratio of variances 
# 0.5639458 

# - 결과 -
# 유의 확률을 이용한 판정 : 유의 확률은 0.002174으로 유의수준 0.05보다 작아
# 귀무가설 기각 > 이분산 가정 두집단

# 표본 추출의 남녀 비율

# Wr.Hnd, 결측치 제거
Wr.Hnd.NOT.NA.data <- MASS::survey[!MASS::survey$Wr.Hnd %>% is.na(),]
Wr.Hnd.NOT.NA.data <- Wr.Hnd.NOT.NA.data[!Wr.Hnd.NOT.NA.data$Sex %>% is.na(),]

# 수 확인
Wr.Hnd.NOT.NA.data %>% 
  group_by(Sex) %>%
  summarise(no_rows = length(Sex))
# 1 Female     118
# 2 Male       117

# 표준평균 확인
Wr.Hnd.NOT.NA.data %>% 
  group_by(Sex) %>% 
  summarise(mean(Wr.Hnd))
# Sex    `mean(Wr.Hnd)`
# <fct>           <dbl>
# 1 Female           17.6
# 2 Male             19.7

# 표준편차 확인
Wr.Hnd.NOT.NA.data %>% 
  group_by(Sex) %>% 
  summarise(sd(Wr.Hnd))
# # A tibble: 3 x 2
# Sex    `sd(Wr.Hnd)`
# <fct>         <dbl>
# 1 Female         1.31
# 2 Male           1.75

Wr.Hnd.m.mean <- 19.7
Wr.Hnd.f.mean <- 17.6


t.test(Wr.Hnd.NOT.NA.data$Wr.Hnd ~ Wr.Hnd.NOT.NA.data$Sex,
       mu = Wr.Hnd.m.mean - Wr.Hnd.f.mean,
       var.equal = FALSE)
# Welch Two Sample t-test
# 
# data:  Wr.Hnd.NOT.NA.data$Wr.Hnd by Wr.Hnd.NOT.NA.data$Sex
# t = -21.009, df = 215.27, p-value < 2.2e-16
# alternative hypothesis: true difference in means between group Female and group Male is not equal to 2.1
# 95 percent confidence interval:
#   -2.544482 -1.747753
# sample estimates:
#   mean in group Female   mean in group Male 
# 17.59576             19.74188 

ggplot(Wr.Hnd.NOT.NA.data, aes(x=Sex, y=Wr.Hnd)) + 
  geom_boxplot() + 
  stat_summary(fun = mean, geom = "point", shape = 20, size = 8, color = 'red') + 
  theme(legend.position = "none")

# ------------------------------------------- #



# ------------------------------------------- #
# 2)
# 귀무가설 : 남자와 여자의 NW.Hnd 주손 반대 손(주로 사용하지 않는 손)의 한뼘 길이는 동일하지 않다.
# 대립가설 : 남자와 여자의 NW.Hnd 주손 반대 손의 한뼘 길이는 동일하다.


# ------------------------------#
# NW.Hnd 등분산, 이분산 확인
# 가설 수립 : 두 집단의 분산이 동일하면 그 비가 1
# 귀무 가설 : 두 집단의 분산은 서로 동일하다.
# 대립 가설 : 두 집단의 분산은 서로 동일하지 않다.

# 동일성 검정
var.test(MASS::survey$NW.Hnd ~ MASS::survey$Sex)
#     F test to compare two variances
# 
# data:  MASS::survey$NW.Hnd by MASS::survey$Sex
# F = 0.6086, num df = 117, denom df = 116, p-value = 0.00777
# alternative hypothesis: true ratio of variances is not equal to 1
# 95 percent confidence interval:
#   0.4224428 0.8765344
# sample estimates:
#   ratio of variances 
# 0.6085991 

# NW.Hnd, 결측치 제거
NW.Hnd.NOT.NA.data <- MASS::survey[!MASS::survey$NW.Hnd %>% is.na(),]
NW.Hnd.NOT.NA.data <- NW.Hnd.NOT.NA.data[!NW.Hnd.NOT.NA.data$Sex %>% is.na(),]

# 수 확인
NW.Hnd.NOT.NA.data %>% 
  group_by(Sex) %>%
  summarise(no_rows = length(Sex))
# 1 Female     118
# 2 Male       117

# 표준평균 확인
NW.Hnd.NOT.NA.data %>% 
  group_by(Sex) %>% 
  summarise(mean(NW.Hnd))
# Sex    `mean(Wr.Hnd)`
# <fct>           <dbl>
# 1 Female           17.5
# 2 Male             19.7

# 표준편차 확인
NW.Hnd.NOT.NA.data %>% 
  group_by(Sex) %>% 
  summarise(sd(NW.Hnd))
# # A tibble: 3 x 2
# Sex    `sd(Wr.Hnd)`
# <fct>         <dbl>
# 1 Female         1.41
# 2 Male           1.80

NW.Hnd.f.mean <- 17.5
NW.Hnd.m.mean <- 19.7


t.test(NW.Hnd.NOT.NA.data$NW.Hnd ~ NW.Hnd.NOT.NA.data$Sex,
       mu = NW.Hnd.m.mean - NW.Hnd.f.mean,
       var.equal = FALSE)
# Welch Two Sample t-test
# 
# data:  NW.Hnd.NOT.NA.data$NW.Hnd by NW.Hnd.NOT.NA.data$Sex
# t = -21.101, df = 219.13, p-value < 2.2e-16
# alternative hypothesis: true difference in means between group Female and group Male is not equal to 2.2
# 95 percent confidence interval:
#   -2.67411 -1.84139
# sample estimates:
#   mean in group Female   mean in group Male 
# 17.45678             19.71453 

ggplot(NW.Hnd.NOT.NA.data, aes(x=Sex, y=NW.Hnd)) + 
  geom_boxplot() + 
  stat_summary(fun = mean, geom = "point", shape = 20, size = 8, color = 'red') + 
  theme(legend.position = "none")

# ------------------------------------------- #


# ---------------------------------------------------------------- #
# 자유롭게 변수들을 선택하여 분산분석(ANOVA)
# (각 2개씩)
#   - 귀무가설과 대립가설 설정
#   - 분석할 변수들을 바탕으로 교차 테이블(분할표) 작성하기
#   - 유의수준 (0.05)를 기준으로 통계적 의사결정 (검정통계량과 p-value를 확인)

# ------------------------------------------- #
# 1)
# 귀무 가설 : 흡연 강도에 따라서 주손 한뼘의 길이는 차이가 없다.(동일 하다.)
# 대립 가설 : 흡연 강도에 따라서 주손 한뼘의 길이는 차이가 있다.(동일 하지 않다.)

# 흡연 강도 결측치 제거
smoke.NOT.NA.data <- MASS::survey[!MASS::survey$Smoke %>% is.na(),]
smoke.NOT.NA.data <- smoke.NOT.NA.data[!smoke.NOT.NA.data$Wr.Hnd %>% is.na(),]

# 수 확인
smoke.NOT.NA.data %>% 
  group_by(Smoke) %>%
  summarise(no_rows = length(Smoke))
# # A tibble: 4 x 2
# Smoke no_rows
# <fct>   <int>
# 1 Heavy      11
# 2 Never     188
# 3 Occas      19
# 4 Regul      17


# 표준평균 확인
smoke.NOT.NA.data %>% 
  group_by(Smoke) %>% 
  summarise(mean(Wr.Hnd))
# # A tibble: 4 x 2
# Smoke `mean(Wr.Hnd)`
# <fct>          <dbl>
# 1 Heavy           19.2
# 2 Never           18.6
# 3 Occas           18.4
# 4 Regul           19.5


# 표준편차 확인
smoke.NOT.NA.data %>% 
  group_by(Smoke) %>% 
  summarise(sd(Wr.Hnd))
# # A tibble: 4 x 2
# Smoke `sd(Wr.Hnd)`
# <fct>        <dbl>
# 1 Heavy         2.75
# 2 Never         1.72
# 3 Occas         2.20
# 4 Regul         2.41

ow1 <- lm(Wr.Hnd ~ Smoke, data=smoke.NOT.NA.data)
# Call:
#   lm(formula = Wr.Hnd ~ Smoke, data = smoke.NOT.NA.data)
# 
# Coefficients:
#   (Intercept)   SmokeNever   SmokeOccas   SmokeRegul  
# 19.2182      -0.6379      -0.8129       0.2348  

anova(ow1)
# Analysis of Variance Table
# 
# Response: Wr.Hnd
# Df Sum Sq Mean Sq F value Pr(>F)
# Smoke       3  16.54  5.5142  1.5771 0.1957
# Residuals 231 807.69  3.4965  

ggplot(smoke.NOT.NA.data, aes(x=Smoke, y=Wr.Hnd)) + 
  geom_boxplot() + 
  stat_summary(fun = mean, geom = "point", shape = 20, size = 8, color = 'red') + 
  theme(legend.position = "none")

# ------------------------------------------- #
# 2)
# 귀무 가설 : 일상 운동 횟수에 따라서 주손 한뼘의 길이는 차이가 없다.(동일 하다.)
# 대립 가설 : 일상 운동 횟수에 따라서 주손 한뼘의 길이는 차이가 있다.(동일 하지 않다.)

# 흡연 강도 결측치 제거
exer.NOT.NA.data <- MASS::survey[!MASS::survey$Exer %>% is.na(),]
exer.NOT.NA.data <- exer.NOT.NA.data[!exer.NOT.NA.data$Wr.Hnd %>% is.na(),]

# 수 확인
exer.NOT.NA.data %>% 
  group_by(Exer) %>%
  summarise(no_rows = length(Exer))
# Exer  no_rows
# <fct>   <int>
# 1 Freq      115
# 2 None       23
# 3 Some       97

# 표준평균 확인
exer.NOT.NA.data %>% 
  group_by(Exer) %>% 
  summarise(mean(Wr.Hnd))
# # A tibble: 3 x 2
# Exer  `mean(Wr.Hnd)`
# <fct>          <dbl>
# 1 Freq            18.8
# 2 None            18.7
# 3 Some            18.5


# 표준편차 확인
exer.NOT.NA.data %>% 
  group_by(Exer) %>% 
  summarise(sd(Wr.Hnd))
# # A tibble: 3 x 2
# Exer  `sd(Wr.Hnd)`
# <fct>        <dbl>
# 1 Freq          2.08
# 2 None          1.76
# 3 Some          1.63

ow2 <- lm(Wr.Hnd ~ Exer, data=exer.NOT.NA.data)
# Call:
#   lm(formula = Wr.Hnd ~ Exer, data = exer.NOT.NA.data)
# 
# Coefficients:
#   (Intercept)     ExerNone     ExerSome  
# 18.8478      -0.1687      -0.3932   

anova(ow2)
# Analysis of Variance Table
# 
# Response: Wr.Hnd
# Df Sum Sq Mean Sq F value Pr(>F)
# Exer        2   8.14  4.0686  1.1539 0.3172
# Residuals 233 821.55  3.5260 

ggplot(smoke.NOT.NA.data, aes(x=Exer, y=Wr.Hnd)) + 
  geom_boxplot() + 
  stat_summary(fun = mean, geom = "point", shape = 20, size = 8, color = 'red') + 
  theme(legend.position = "none")

# ---------------------------------------------------------------- #


# ---------------------------------------------------------------- #
# 자유롭게 변수들을 선택하여 (적합성) 검정을 실시
#   - 귀무가설과 대립가설 설정
#   - 분석할 변수들을 바탕으로 교차 테이블(분할표) 작성하기
#   - 유의수준 (0.05)를 기준으로 통계적 의사결정 (검정통계량과 p-value를 확인)


# ------------------------------------------- #
# 1) 흡연자의 기대(이론)분포는 다음과 같이 나온다.
# URL : https://kosis.kr/search/search.do?query=%ED%9D%A1%EC%97%B0
# 흡연자의 비율 20.6%
# 귀무 가설 : 흡연자의 관측 분포와 기대(이론)분포가 동일하다.
# 대립 가설 : 흡연자의 관측 분포와 기대(이론)분포가 동일하지 않다.
smoke.NOT.NA.data <- MASS::survey[!MASS::survey$Smoke %>% is.na(),]

# 수 확인
smoke.NOT.NA.data %>% 
  group_by(Smoke) %>%
  summarise(no_rows = length(Smoke))
# 1 Heavy      11
# 2 Never     189
# 3 Occas      19
# 4 Regul      17

obs1 <- c(47, 189)
null.probs1 <- c(2/10, 8/10)
chisq_out1 <- chisq.test(obs1, p = null.probs1)
# Chi-squared test for given probabilities
# 
# data:  obs1
# X-squared = 0.0010593, df = 1, p-value = 0.974



# ------------------------------------------- #
# 2) 왼손, 오른손 잡이의 기대(이론)분포는 다음과 같이 나온다.
# URL : https://ko.wikipedia.org/wiki/%EC%98%A4%EB%A5%B8%EC%86%90%EC%9E%A1%EC%9D%B4
# 귀무 가설 : 주손 잡이의 관측 분포와 기대(이론)분포가 동일하다.
# 대립 가설 : 주손 잡이의 관측 분포와 기대(이론)분포가 동일하지 않다.

W.Hnd.NOT.NA.data <- MASS::survey[!MASS::survey$W.Hnd %>% is.na(),]

# 수 확인
W.Hnd.NOT.NA.data %>% 
  group_by(W.Hnd) %>%
  summarise(no_rows = length(W.Hnd))
# 1 Left       18
# 2 Right     218

obs2 <- c(218, 18)
null.probs2 <- c(85/100, 15/100)
chisq_out2 <- chisq.test(obs2, p = null.probs2)
# Chi-squared test for given probabilities
# 
# data:  obs2
# X-squared = 10.062, df = 1, p-value = 0.001514

# ---------------------------------------------------------------- #

# ---------------------------------------------------------------- #
# 자유롭게 변수들을 선택하여 (독립성) 검정을 실시
#   - 귀무가설과 대립가설 설정
#   - 분석할 변수들을 바탕으로 교차 테이블(분할표) 작성하기
#   - 유의수준 (0.05)를 기준으로 통계적 의사결정 (검정통계량과 p-value를 확인)

# ------------------------------------------- #
# 1) 흡연자의 비율 남녀별로 조사
# 귀무 가설 : 남, 녀의 흡연 강도는 독립적이다.
# 대립 가설 : 남, 녀의 흡연 강도는 독립적이지 않다.

Sex.NOT.NA.data <- MASS::survey[!MASS::survey$Sex %>% is.na(),]
Sex.NOT.NA.data <- Sex.NOT.NA.data[!Sex.NOT.NA.data$Smoke %>% is.na(),]


# 수 확인
data_matrix1 <- cbind(
  Sex.NOT.NA.data[Sex.NOT.NA.data$Smoke == "Never",] %>% 
    group_by(Sex) %>% summarise(no_rows = length(Sex)) %>% pull("no_rows"),
  
  Sex.NOT.NA.data[Sex.NOT.NA.data$Smoke %in% c("Heavy", "Occas", "Regul"),] %>% 
    group_by(Sex) %>% summarise(no_rows = length(Sex)) %>% pull("no_rows")
)
#      [,1] [,2]
# [1,]   99   19
# [2,]   89   28

dimnames(data_matrix1) <- list("Sex" = c("Female", "Male"), 
                              "Smoke" = c("non-smoker", "smoker"))

# Smoke
# Sex      non-smoker smoker
# Female         99     19
# Male           89     28


addmargins(data_matrix1)
# Smoke
# Sex      non-smoker smoker Sum
# Female         99     19 118
# Male           89     28 117
# Sum           188     47 235


prop.table(data_matrix1)
# Smoke
# Sex      non-smoker     smoker
# Female  0.4212766 0.08085106
# Male    0.3787234 0.11914894


addmargins(prop.table(data_matrix1))
# Smoke
# Sex      non-smoker     smoker       Sum
# Female  0.4212766 0.08085106 0.5021277
# Male    0.3787234 0.11914894 0.4978723
# Sum     0.8000000 0.20000000 1.0000000

null.matrix.probs1 <- rbind(
  c(6/100, 94/100),
  c(34/100, 66/100)
)

chisq.test(data_matrix1, p = null.matrix.probs1)
# Pearson's Chi-squared test with Yates' continuity correction
# 
# data:  data_matrix1
# X-squared = 1.7883, df = 1, p-value = 0.1811


# ------------------------------------------- #
# 1) 주 손잡이 남녀별로 조사
# 귀무 가설 : 남, 녀의 왼손잡이 오른손 잡이 비율은 독립적이다.
# 대립 가설 : 남, 녀의 왼손잡이 오른손 잡이 비율은 독립적이지 않다.

Sex.NOT.NA.data <- MASS::survey[!MASS::survey$Sex %>% is.na(),]
Sex.NOT.NA.data <- Sex.NOT.NA.data[!Sex.NOT.NA.data$W.Hnd %>% is.na(),]

# 수 확인
data_matrix2 <- cbind(
  Sex.NOT.NA.data[Sex.NOT.NA.data$W.Hnd == "Left",] %>% 
    group_by(Sex) %>% summarise(no_rows = length(Sex)) %>% pull("no_rows"),
  
  Sex.NOT.NA.data[Sex.NOT.NA.data$W.Hnd == "Right",] %>% 
    group_by(Sex) %>% summarise(no_rows = length(Sex)) %>% pull("no_rows")
)
#      [,1] [,2]
# [1,]    7  110
# [2,]   10  108

dimnames(data_matrix2) <- list("Sex" = c("Female", "Male"), 
                               "W.Hnd" = c("Left", "Right"))
#               W.Hnd
# Sex      Left Right
# Female    7   110
# Male     10   108

addmargins(data_matrix2)
#          W.Hnd
# Sex      Left Right Sum
# Female    7   110 117
# Male     10   108 118
# Sum      17   218 235


prop.table(data_matrix2)
#               W.Hnd
# Sex            Left     Right
# Female 0.02978723 0.4680851
# Male   0.04255319 0.4595745


addmargins(prop.table(data_matrix2))
#                                   W.Hnd
# Sex            Left     Right       Sum
# Female  0.02978723 0.4680851 0.4978723
# Male    0.04255319 0.4595745 0.5021277
# Sum     0.07234043 0.9276596 1.0000000

null.matrix.probs2 <- rbind(
  c(85/100, 15/100),
  c(85/100, 15/100),
)

chisq.test(data_matrix2, p = null.matrix.probs2)
# Pearson's Chi-squared test with Yates' continuity correction
# 
# data:  data_matrix2
# X-squared = 0.23563, df = 1, p-value = 0.6274

# ---------------------------------------------------------------- #


# ---------------------------------------------------------------- #
# 수치형-수치형 변수
# 상관 계수 및 산점도 그리기
# Wr.Hnd(주 손의 한 뼘의 길이) & Height(키)

# $ Age   : num  18.2 17.6 16.9 20.3 23.7 ...
#   나이>  (수치형)

# $ Height: num  173 178 NA 160 165 ...
#   > 키 (수치형)

# $ NW.Hnd: num  18 20.5 13.3 18.9 20 17.7 17.7 17.3 19.5 18.5 ...
#   > 주 손이 아닌 한뼘의 길이 (수치형)

# $ Wr.Hnd: num  18.5 19.5 18 18.8 20 18 17.7 17 20 18.5 ...
#   > 주 손의 한 뼘의 길이 (수치형)


plot(MASS::survey[, c("Age", "Height", "NW.Hnd", "Wr.Hnd")])


cor.table <- cor(drop_na(MASS::survey[, c("Age", "Height", "NW.Hnd", "Wr.Hnd")]))
#                Age      Height     NW.Hnd     Wr.Hnd
# Age     1.00000000 -0.03718289 0.06518843 0.02844991
# Height -0.03718289  1.00000000 0.58412700 0.60099095
# NW.Hnd  0.06518843  0.58412700 1.00000000 0.96293225
# Wr.Hnd  0.02844991  0.60099095 0.96293225 1.00000000

# ---------------------------------------------------------------- #



# ---------------------------------------------------------------- #
# Mushroom Classification
# 유용한 입력 변수 선택하기
#     - 독립성 검정 활용

# 추가 사항> Decision Tree 또는 Random Forest Classifier 구축해 보기
#     - 모든 데이터를 학습데이터로 사용, 하이퍼파라미터는 기본값으로 설정
#     - 주요 입력 변수 추출해 보기

Mushroom.data <- read.csv(file = "C:/Users/gksxo/Desktop/Folder/github/SCH_Multivariate_Analysis/data/8차/과제/mushrooms.csv",
                          header = T)


str(Mushroom.data)
# 'data.frame':	8124 obs. of  23 variables:
# $ class                   : chr  "p" "e" "e" "p" ... < target
# $ cap.shape               : chr  "x" "x" "b" "x" ...
# $ cap.surface             : chr  "s" "s" "s" "y" ...
# $ cap.color               : chr  "n" "y" "w" "w" ...
# $ bruises                 : chr  "t" "t" "t" "t" ...
# $ odor                    : chr  "p" "a" "l" "p" ...
# $ gill.attachment         : chr  "f" "f" "f" "f" ...
# $ gill.spacing            : chr  "c" "c" "c" "c" ...
# $ gill.size               : chr  "n" "b" "b" "n" ...
# $ gill.color              : chr  "k" "k" "n" "n" ...
# $ stalk.shape             : chr  "e" "e" "e" "e" ...
# $ stalk.root              : chr  "e" "c" "c" "e" ...
# $ stalk.surface.above.ring: chr  "s" "s" "s" "s" ...
# $ stalk.surface.below.ring: chr  "s" "s" "s" "s" ...
# $ stalk.color.above.ring  : chr  "w" "w" "w" "w" ...
# $ stalk.color.below.ring  : chr  "w" "w" "w" "w" ...
# $ veil.type               : chr  "p" "p" "p" "p" ...
# $ veil.color              : chr  "w" "w" "w" "w" ...
# $ ring.number             : chr  "o" "o" "o" "o" ...
# $ ring.type               : chr  "p" "p" "p" "p" ...
# $ spore.print.color       : chr  "k" "n" "n" "k" ...
# $ population              : chr  "s" "n" "n" "s" ...
# $ habitat                 : chr  "u" "g" "m" "u" ...

# Factor 형으로 변경
for (col.name in colnames(Mushroom.data)) {
  Mushroom.data[,col.name] <- as.factor(Mushroom.data[,col.name])  
}


# 타겟 변수와, 그외 변수 백터 생성
target <- "class"
variables <- colnames(Mushroom.data)[!(colnames(Mushroom.data) %in% c(target))]



col.data <- function(data, col.name, factor.var) {
  # 식용버섯, 독버섯 개수 추출 함수 #
  return(
    data[data %>% select(col.name) == factor.var,] %>% 
      group_by(class, .drop = FALSE) %>% 
      summarise("tmp.col" = length(class)) %>% 
      pull("tmp.col")
  )
}


# 모든 데이터 전처리

result.vector <- c()
for (col.name in variables) {
  tmp.vector <- c()
  for (factor.var in unique(Mushroom.data[,col.name])) {
    tmp.col.data <- col.data(data = Mushroom.data, col.name, factor.var)
    tmp.vector <- c(tmp.vector, tmp.col.data)
  }
  tmp.matrix <- tmp.vector %>% 
    matrix(nrow=2)
  dimnames(tmp.matrix) <- list(
    "class" = c("Edible", "Poisonous"),
    "variables" = unique(Mushroom.data[,col.name])
  )
  chisq.test_output <- tmp.matrix %>% chisq.test()
  
  # print(col.name)
  # chisq.test_output$observed %>% print()
  # print("residuals")
  # chisq.test_output$residuals %>% print()
  # print("statistic")
  # chisq.test_output$statistic %>% print()
  # print("parameter")
  # chisq.test_output$parameter %>% print()
  # print("p.value")
  # chisq.test_output$p.value %>% print()
  # 
  # chisq.test_output %>% print()
  
  result.vector <- c(result.vector, 
                     chisq.test_output$statistic, 
                     chisq.test_output$parameter, 
                     chisq.test_output$p.value,
                     chisq.test_output$p.value >= 0.05)
}

result.matrix <- result.vector %>% 
                    matrix(nrow = variables %>% length(), byrow = TRUE)

dimnames(result.matrix) <- list(
  "variables" = variables,
  "result" = c("X-squared", "df", "p-value", "h0")
)

# result
# variables                   X-squared df       p-value h0
  # cap.shape                 489.91995  5 1.196457e-103  0
  # cap.surface               315.04283  3  5.518427e-68  0
  # cap.color                 387.59777  9  6.055815e-78  0
  # bruises                  2041.41565  1  0.000000e+00  0
  # odor                     7659.72674  8  0.000000e+00  0
  # gill.attachment           133.98618  1  5.501707e-31  0
  # gill.spacing              984.14333  1 5.022978e-216  0
  # gill.size                2366.83426  1  0.000000e+00  0
  # gill.color               3765.71409 11  0.000000e+00  0
  # stalk.shape                84.14204  1  4.604746e-20  0
  # stalk.root               1344.44053  4 7.702048e-290  0
  # stalk.surface.above.ring 2808.28629  3  0.000000e+00  0
  # stalk.surface.below.ring 2684.47408  3  0.000000e+00  0
  # stalk.color.above.ring   2237.89850  8  0.000000e+00  0
  # stalk.color.below.ring   2152.39089  8  0.000000e+00  0
  # veil.type                  10.49532  1  1.196771e-03  0
  # veil.color                191.22370  3  3.320973e-41  0
  # ring.number               374.73683  2  4.235758e-82  0
  # ring.type                2956.61928  4  0.000000e+00  0
  # spore.print.color        4602.03317  8  0.000000e+00  0
  # population               1929.74089  5  0.000000e+00  0
  # habitat                  1573.77726  6  0.000000e+00  0

# 정렬
result.matrix.order <- result.matrix[order(result.matrix[,"X-squared"], decreasing=TRUE),]
# result
# variables                   X-squared df       p-value h0
# odor                     7659.72674  8  0.000000e+00  0
# spore.print.color        4602.03317  8  0.000000e+00  0
# gill.color               3765.71409 11  0.000000e+00  0
# ring.type                2956.61928  4  0.000000e+00  0
# stalk.surface.above.ring 2808.28629  3  0.000000e+00  0
# stalk.surface.below.ring 2684.47408  3  0.000000e+00  0
# gill.size                2366.83426  1  0.000000e+00  0
# stalk.color.above.ring   2237.89850  8  0.000000e+00  0
# stalk.color.below.ring   2152.39089  8  0.000000e+00  0
# bruises                  2041.41565  1  0.000000e+00  0
# population               1929.74089  5  0.000000e+00  0
# habitat                  1573.77726  6  0.000000e+00  0
# stalk.root               1344.44053  4 7.702048e-290  0
# gill.spacing              984.14333  1 5.022978e-216  0
# cap.shape                 489.91995  5 1.196457e-103  0
# cap.color                 387.59777  9  6.055815e-78  0
# ring.number               374.73683  2  4.235758e-82  0
# cap.surface               315.04283  3  5.518427e-68  0
# veil.color                191.22370  3  3.320973e-41  0
# gill.attachment           133.98618  1  5.501707e-31  0
# stalk.shape                84.14204  1  4.604746e-20  0
# veil.type                  10.49532  1  1.196771e-03  0


# RF 모델 사용
model_rf <- randomForest(class ~ ., data = Mushroom.data)

# 변수 중요도 추출
model_rf.importance <- randomForest::importance(model_rf)


model_rf.importance.table <- model_rf.importance[model_rf.importance %>% 
                      order(decreasing=TRUE),] %>% 
  as.data.frame()


colnames(model_rf.importance.table) <- "MeanDecreaseGini"
#                          MeanDecreaseGini
# odor                          1383.128838
# spore.print.color              623.888288
# gill.color                     339.616202
# gill.size                      247.349905
# stalk.surface.above.ring       213.599773
# stalk.surface.below.ring       181.139158
# ring.type                      177.689985
# population                     145.176697
# habitat                        111.684407
# stalk.root                     109.693181
# bruises                         97.196142
# gill.spacing                    77.408949
# cap.color                       63.523633
# stalk.color.above.ring          61.547539
# stalk.color.below.ring          59.613151
# stalk.shape                     50.933536
# ring.number                     50.213715
# cap.surface                     28.462752
# cap.shape                       13.944580
# veil.color                       3.493644
# gill.attachment                  1.676824
# veil.type                        0.000000












