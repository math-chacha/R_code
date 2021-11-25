# 윤종식, 『ADP 실기 데이터 분석 전문가』, (주)데이터에듀-2020, 318-325


# 연관분석

## 1. 연관분석이란
##    항목(item)들 관계를 if-then 형식으로 찾아가는 분석방법
##    일종의 규칙 기반 학습방법으로 마케팅에서 고객의 상품 구매 데이터 활용해 품목 간 연관성 알아볼 때 사용
##    측도
##    1) 지지도(support) = 전체 거래 중 항목 A와 항목 B를 동시에 포함하는 거래의 비율
##                        P(A intersection B)
##    2) 신뢰도(confidence) = 항목 A를 포함한 거래 중에서 항목 A와 항목 B가 같이 포함될 확률
##                           support / P(A)
##    3) 향상도(lift) = 항목 A가 구매되지 않았을 때 항목 B의 구매확률에 비해 A가 구매되었을 때 B의 구매확률 증가비
##                      confidence / P(B)
##    알고리즘(Apriori)
##    빈발항목집합 : 최소 지지도보다 큰 지지도 값을 갖는 품목의 집합
##    Apriori 알고리즘은 최소 지지도 이상의 빈발항목집합을 찾은 후 그것들에 대해서만 연관규칙을 계산해 연산량 감소 시킨 알고리즘

##    R code
##    as(data, class, ...)  # 연관분석을 위해 데이터를 "transactions" class로 변경 필요
##    arules::apriori(data, parameter, appearance, control)  data : 트랜잭션 데이터 
##    arules::inspect(x,...) : 연관분석 결과 확인 x : 연관규칙 또는 트랜잭션 또는 아이템 매트릭스 데이터
##    example(as함수 사용)
library(arules)
#데이터 입력
id <- c(1, 2, 3, 4, 5, 6)
gender <- c("FEMALE", "MALE", "FEMALE", "FEMALE", "MALE", "FEMALE")
age <- c("age_20", "age_20", "age_40", "age_30", "age_40", "age_30")
rank <- c("Gold", "Silver", "Silver", "VIP", "Gold", "Gold")
mobile_app_use <- c("YES", "YES", "NO", "YES", "NO", "YES")
re_order <- c("YES", "NO", "NO", "YES", "NO", "YES")

cust_tel <- cbind(id, gender, age, rank, mobile_app_use, re_order)
cust_tel <- as.data.frame(cust_tel)
library(dplyr)
cust_tel_1 <- cust_tel %>% select(-id)

#as 함수를 활용한 데이터 변형
tran.cust<-as(cust_tel_1,"transactions")
tran.cust # 6 transactions / 12 items

#데이터 확인하기
inspect(tran.cust)


##    example(연관분석)
library(arules)
data(Groceries)
Groceries # 9835 transactions / 169 items

inspect(Groceries[1:3]) # example

# 분석
rules <- apriori(Groceries,
                 parameter=list(support=0.01,  # 최소 지지도
                                confidence=0.3)) # 최소 신뢰도
# 125개 연관규칙 생성

# 결과 확인
inspect(sort(rules,by=c("confidence"), decreasing=T)[1:5]) # 연관규칙 중 신뢰도 내림차순으로 5개 확인


# 좌항 -> 우항 / 우항 -> 좌항 규칙이 겹치는 경우가 있어 없애야함(함수 사용)
prune.dup.rules <- function(rule){
    rule.subset.matrix <- is.subset(rules, rules, sparse=F)
    rule.subset.matrix[lower.tri(rule.subset.matrix, diag=T)] <- NA   # lower.tri 하삼각행렬 NA로(대각원소 포함)
    dup.rules <- colSums(rule.subset.matrix, na.rm=T) >= 1 # 1보다 크거나 같은 것 -> 중복 존재하는 것
    pruned.rules <- rules[!dup.rules]
    return (pruned.rules)
}

# 특정 규칙 찾기 (우변 아이템 구매를 이끌 아이템 세트 찾기)
metric.params <- list(supp=0.001, conf=0.5, minlen=2) # minlen : 좌항과 우항을 합친 최소 물품수 
rules <- apriori(data = Groceries, parameter = metric.params,
                 appearance = list(default = "lhs", rhs = "soda"), # 우항이 soda인 규칙
                 control = list(verbose = F))
rules <- prune.dup.rules(rules) # 중복 제거
rules <- sort(rules, decreasing=T, by="confidence") # 규칙을 신뢰도 기준으로 정렬
inspect(rules[1:5]) # 상위 5개 표시

# 특정 규칙 찾기 (좌변 아이템 구매를 이끌 아이템 세트 찾기)
metric.params <- list(supp=0.001, conf=0.3, minlen=2) # minlen : 좌항과 우항을 합친 최소 물품수 
rules <- apriori(data = Groceries, parameter = metric.params,
                 appearance = list(default = "rhs", lhs = c("yogurt","sugar")), # 좌항이 yogurt 혹은 sugar인 규칙
                 control = list(verbose = F))
rules <- prune.dup.rules(rules) # 중복 제거
rules <- sort(rules, decreasing=T, by="confidence") # 규칙을 신뢰도 기준으로 정렬
inspect(rules[1:5]) # 상위 5개 표시