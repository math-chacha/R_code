# 윤종식, 『ADP 실기 데이터 분석 전문가』, (주)데이터에듀-2020, 162-171

# 분산분석

## 1. 일원배치 분산분석
##    두 개 이상의 집단(1개의 독립변수)에서 그룹 평균 간 차아가 유의한지 분석
##    가정
##    각 집단 관측치는 서로 독립이며 정규분포
##    각 집단 관측치의 등분산성
##    가설 설정
##    H_0 : k개 집단 간 평균 차이는 없다
##    H_1 : k개 집단의 평균이 모두 같지는 않다
##    사후 검정
##    차이가 유의하다고 판단된 경우 사후 검정을 통해 어떤 집단들이 차이가 있는지 확인해야함
##    Duncan의 Multiple Range Test / Fisher의 최소유의차(LSD) / Tukey의 HSD / Scheffe 방법 등 활용

##    R code
##    분산분석 : aov(formula, data)
##    사후검정 : TukeyHSD(x, conf.level=0.95,...)
##    example (iris Species 별 Sepal.Length 평균 차이가 있는지)
iris_oneway_aov <- aov(Sepal.Length~Species, data=iris)
summary(iris_oneway_aov) # p-value : 2e-16 -> 귀무가설 기각 -> Species 별 Sepal.Length 평균 차이가 있다
TukeyHSD(iris_oneway_aov) # virginica - versicolor - setosa 순으로 Sepal.Length 평균이 큼(diff보고 판단)


## 2. 이원배치 분산분석
##    종속 변수에 대해 두 집단(2개의 독립변수)의 영향을 알아보는 분석
##    교호작용 고려 필요!
##    가정
##    각 집단 관측치의 분포는 정규분포
##    집단 간 관측치는 등분산성
##    가설 설정
##    - 주효과 검정
##    H_0 : A 변수에 따른 종속변수값에는 차이가 없다(A 변수의 효과가 없다)
##    H_1 : A 변수에 따른 종속변수값에는 차이가 있다(A 변수의 효과가 있다)
##    H_0 : B 변수에 따른 종속변수값에는 차이가 없다(A 변수의 효과가 없다)
##    H_1 : B 변수에 따른 종속변수값에는 차이가 있다(A 변수의 효과가 있다)
##    - 교호작용효과 검정
##    H_0 : A 변수와 B 변수 간 상호작용 효과가 없다
##    H_1 : A 변수와 B 변수 간 상호작용 효과가 있다

##    R code
##    aov(formula, data)  formula :  종속변수 ~ A*B
##    interaction.plot(x.factor, trace.factor, response)   x.factor : x축 그룹 변수(A) / trace.factor : 그래프로 표현할 그룹 변수(B)
##    interaction.plot이 교차하면 교호작용 존재한다고 생각할 수 있음
##    example(ToothGrowth 데이터의 len(치아 길이)에 supp(보충제 타입)과 dose(보충제 투여 정도) 두 개 독립변수 효과가 있는지)
str(ToothGrowth)
ToothGrowth$dose_factor <- as.factor(ifelse(ToothGrowth$dose == 0.5, "D0.5", ifelse(ToothGrowth$dose == 1,"D1","D2")))
tooth_len_aov <- aov(len~supp*dose_factor, data=ToothGrowth)
summary(tooth_len_aov) # supp p-value : 0.000231 / dose_factor p-value : 2e-16 / supp*dose_factor : 0.021860 
                       # 주효과 및 교호작용 효과 모두 유의하다
interaction.plot(ToothGrowth$dose_factor, ToothGrowth$supp, ToothGrowth$len, col=c("red","blue","green")) # D2에서 supp가 교차해 교호작용 유의 예상 가능

# 유의하게 나온 효과들로 사후검정(Tukey HSD / Duncan LSR 활용)
TukeyHSD(tooth_len_aov)
