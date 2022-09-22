# ---------------------------------------------------------------- #
# 2022-09-22 (목요일) 8차 다변량 분석 강의
# 내용 : 범주형 데이터 분석 과제
# 
#                                                 - 김재윤 교수님 -  
# ---------------------------------------------------------------- #


# ---------------------------------------------------------------- #
# 사용 데이터
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
#     1 : "Imperial" (feet/inches)
#     2 : "Metric" (centimetres/metres)

# $ Age   : num  18.2 17.6 16.9 20.3 23.7 ...
#   나이>  (수치형)
