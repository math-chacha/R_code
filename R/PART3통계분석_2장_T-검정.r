# 윤종식, 『ADP 실기 데이터 분석 전문가』, (주)데이터에듀-2020, 143-153


# T-검정


## 1. 일표본 T-검정
##    단일모집단에서 관심 있는 연속형 변수의 평균값을 특정 기준값과 비교할 때 사용하는 검정방법
##    가정 : 모집단 원소들의 정규성(shapiro.test 활용)
##    가설 설정
##    H_0 : 모평균의 값은 OO다.
##    H_1 : 모평균의 값은 OO이 아니다.(양측)
##    H_1 : 모평균의 값은 OO보다 작다.(좌단측)
##    H_1 : 모평균의 값은 OO보다 크다.(우단측)

##    R code
##    1) 정규성 검정
##       shapiro.test(data)   기각 시 정규분포 만족 X

##    2) T검정
##       정규분포 만족 -> t.test(x, alternative=c("two.sided","less","greater"), mu=0)
##       정규분포 불만족 -> wilcox.test(x, alternative=c("two.sided","less","greater"), mu=0)
##    example (iris Sepal.Length 평균이 5.5인지)
shapiro.test(iris$Sepal.Length) # p-value : 0.01018 -> 정규분포 불만족
wilcox.test(iris$Sepal.Length, alternative="two.sided",mu=5.5) # p-value : 9.105e-06 -> 평균 5.5라는 귀무가설 기각!


## 2. 대응표본 T-검정
##    단일모집단에 대해 두 번의 처리 가했을 때, 두 처리에 따른 평균 차이 비교
##    가정 : 모집단 원소들의 정규성
##    가설 설정
##    H_0 : 두 모평균의 차이는 0이다.

##    R code
##    1) 대응표본 T검정
##    t.test(x, y, alternative=c("two.sided","less","greater"), paired=FALSE, m=0)
##    example (before/after 좌단측 대응표본 T-검정)
df <- data.frame(before = c(2,1,3,4,5,2,1),
                 after = c(3,1,5,5,5,4,5))
t.test(df$before, df$after, alternative="less",paired=TRUE) # p-vale : 0.01767 -> before/after 평균 차이가 없다는 귀무가설 기각!(==before -> after 값이 커졌다)


## 3. 독립표본 T-검정
##    두 독립된 모집단의 평균 비교
##    가정 : 두 모집단의 정규성 / 서로 독립 / 등분산성 가정 확인필요
##    가설 설정
##    H_0 : 두 모평균의 차이는 0이다.

##    R code
##    1) 등분산성 검정
##       var.test(x, y, data, alternative)   / var.test(formula, data, alternative)

##    2) 독립표본 T검정
##       t.test(x,y, alternative, var.equal=FALSE) / t.test(formula, data, alternative, var.equal=FALSE)

##    example (iris Sepal.Length/Sepal.Width 독립표본 검정)
var.test(iris$Sepal.Length, iris$Sepal.Width) # p-value : 3.597e-14 -> 등분산 기각! 이분산성
t.test(iris$Sepal.Length, iris$Sepal.Width, alternative="two.sided",val.equal=FALSE) # p-value : 2.2e-16 -> 귀무가설 기각! 두 모집단 평균 차이가 있다

