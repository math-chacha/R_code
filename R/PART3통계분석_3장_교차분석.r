# 윤종식, 『ADP 실기 데이터 분석 전문가』, (주)데이터에듀-2020, 154-161

# 교차분석


## 1. 교차분석 개념
##    범주형인 두 변수 간 관계를 알아보는 분석 기법
##    Cross Table을 활용해 분석

## 2. 적합성 검정
##    관측값들이 예상 분포와 일치하는지 검정
##    H_0 : 실제 분포와 이론적 분포는 같다
##    H_1 : 실제 분포와 이론적 분포 간 차이가 있다

##    R code
##    chisq.test(x,y,p)  p : 귀무가설을 통해 설정한 확률값
##    example(survey 데이터 왼손잡이 비율이 15%인지)
library(MASS)
str(survey)  # W.Hnd : 왼손잡이인이 오른손잡이인지
table(survey$W.Hnd) # Left : 18 , Right : 218
chisq.test(table(survey$W.Hnd),p=c(0.15,0.85)) # p-value : 0.001514 -> 귀무가설 기각! 왼손잡이 비율 15%라 할 수 없다


## 3. 독립성 검정
##    모집단이 두 개의 범주로 구분되어있을 때 두 범주 간 관계가 독립인지 검정
##    H_0 : 두 범주는 독립이다
##    H_1 : 두 범주는 독립이 아니다

##    R code
##    xtabs(formula, data) : 분할표 그려주는 함수
##    table(범주1, 범주2)   : 분할표 그려주는 함수
##    둘 중 원하는 것 사용
##    example (survey에서 왼손잡이 여부와 흡연 정도는 독립이다)
table(survey$W.Hnd, survey$Smoke) # Left & (Heavy, Never, Occas, Regul)  : (1,   13,  3,  1)
                                  # Right & (Heavy, Never, Occas, Regul) : (10, 175, 16, 16) 
chisq.test(table(survey$W.Hnd, survey$Smoke)) # p-value : 0.5661 -> 귀무가설 기각 X -> 왼손잡이 여부와 흡연 정도는 독립이다


## 4. 동질성 검정
##    관측값들이 특정 범주 내에서 서로 비슷하게 나타내고 있는지 검정
##    j = 1,2,...,c
##    H_0 : P_1j = P_2j = ... = P_rj (모든 P_nj(n=1,...r)은 동일)
##    H_1 : not H_0

##    R code
##    독립성 검정과 동일
##    example(남학생 여학생 별 선호하는 TV프로그램이 동일하다)  출처: https://rfriend.tistory.com/139
data <- data.frame("성별"=rep(c("남","여"),c(100,200)), "TV프로"=rep(c("A","B","C","A","B","C"),c(50,30,20,50,80,70)))
table(data$성별, data$TV프로) # 남 & (A, B, C) : (50,30,20)
                             # 여 & (A, B, C) : (50,80,70)
chisq.test(table(data$성별, data$TV프로)) # p-value : 6.384e-05 -> 귀무가설 기각 -> 남학생 여학생 별 선호하는 TV프로그램은 동일하지 않다