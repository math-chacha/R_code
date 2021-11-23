# 윤종식, 『ADP 실기 데이터 분석 전문가』, (주)데이터에듀-2020, 172-179

# 상관분석

## 1. 상관분석이란
##    두 변수 간 관계의 정도를 알아보는 분석
##    피어슨의 상관계수, 스피어만 상관계수, 켄달의 순위 상관계수 등이 존재
##    공분산 / 두 변수의 표준편차 곱 = 상관계수


## 2. 상관분석 유형
##    1) 피어슨의 상관계수
##       두 변수 모두 연속형이며 정규성 따른다는 가정 하에 선형적 상관관계 측정
##    2) 스피어만의 상관계수
##       두 변수가 정규성 만족하지 않거나 순위 및 순서 형태로 주어지는 경우 사용하는 상관계수
##       데이터에 순위를 매긴 후 순위에 대한 상관관계를 산출하는 비모수적 방법
##    3) 켄달의 순위상관계수
##       데이터가 (X_i, Y_i)처럼 순서쌍으로 주어졌을 때 X가 커지며 Y도 커지면 부합 아니면 비부합
##       전체 데이터에서 부합쌍/비부합쌍 비율로 상관계수 산출
##       켄달의 순위상관계수 = 1 -> 부합쌍 비율 100%
##       켄달의 순위상관계수 = -1 -> 비부합쌍 비율 100%


## 3. 상관계수 검정
##    변수 간 상관계수에 대해 검정
##    가설 설정
##    H_0 : 두 변수 간 상관관계가 없다
##    H_0 : 두 변수 간 상관관계가 있다

##    R code
##    상관계수 산출
##    cor(x,y, method = c("pearson","kendall","spearman"),use) use : NA처리방법  "everything" : NA존재 시 NA출력 / "all.obs" : NA존재 시 오류 메시지 / "cimplete.obs" : NA제외하고 상관계수 계산 / "pairwise.complete.obs" : 모든 변수 쌍에서 결측치 없는 것만 활용
##    상관검정
##    cor.test(x,y, alternative=c("two.sided","less","greater"), method=c("pearson","kendall","spearman"))
##    상관계수행렬 시각화
##    pairs(x,labels,...) labels : 그래프 제목
##    corrplot(corr, method)
##    example1(airquality Ozone, Solar.R, Wind, Temp의 상관계수 산출)
library(dplyr)
data <- airquality %>% select(Ozone, Solar.R, Wind, Temp)
cor(data, method="pearson", use = "pairwise.complete.obs")
##    example2(그래프)
pairs(cor(data, method="pearson", use = "pairwise.complete.obs"))
##    example3(Ozone과 Wind 상관검정)
cor.test(data$Ozone, data$Wind, method="pearson") # p-value : 9.272e-13 -> 귀무가설 기각 -> Ozone과 Wind 상관관계 존재
                                                  # cor = -0.6015465 로 음의 상관관계를 가지고 있다고 할 수 있음