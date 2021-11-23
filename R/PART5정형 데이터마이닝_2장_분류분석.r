# 윤종식, 『ADP 실기 데이터 분석 전문가』, (주)데이터에듀-2020, 244-299

# 분류분석
# 활용 데이터 : credit 은행 자금 대출 신청한 1000명의 자산 신용 관련 데이터 (credit_final.csv)
# 종속변수 : credit.rating 0 : 불량 / 1: 우량
# 독립변수
# 수치형 : 4개
# 범주형 : 18개

## 1. 로지스틱 회귀분석(Logistic Regression)
##    종속변수가 범주형인 경우 적용하는 회귀분석 모형
##    오즈 = 성공확률 / 실패확률
##    오즈(odds)의 관점에서 해석 가능 exp(beta_1)의 의미는 나머지 변수가 주어질 때 , x_1이 한 단위 증가할 때마다 성공의 오즈가 몇 배 증가하는지 나타냄

##    R code
##    glm(formula, data, family="binomial", ...) : 로지스틱 회귀분석
##    nnet::multinom(formula, data) : 다항로지스틱 회귀분석
##    predict(model, newdata, type, ...) type : "link"(log-odds값), "class"(범주형 값), "response"(0~1확률값)

##    example(로지스틱회귀분석 train:test = 7:3 분할 + 모형 적합)
credit <- read.csv('C:/cha/기타/R/credit_final.csv', header=TRUE, sep=',')
credit$credit.rating <- as.factor(credit$credit.rating)

# 분할
set.seed(123)
idx <- sample(1:nrow(credit), 0.7*nrow(credit), replace=F)
train <- credit[idx,]
test <- credit[-idx,]

# 모형 적합
lr_credit <- glm(credit.rating~., data=train, family="binomial")
summary(lr_credit) # 유의미하지 않은 회귀계수 다수 존재
step_lr_credit <- step(lr_credit,
                       scope = list(lower=~1, upper = ~ account.balance  +  credit.duration.months  +  previous.credit.payment.status  +  
                                                        credit.purpose  +  credit.amount  +  savings  +  employment.duration  +
                                                        installment.rate  +  marital.status  +  guarantor  +  residence.duration  +
                                                        current.assets  +  age  +  other.credits  +  apartment.type  +  bank.credits  +
                                                        occupation  +  dependents  +  telephone  +  foreign.worker),
                        direction="both")
summary(step_lr_credit) # 20개 중 15개 변수 유의
                        # P(credit.rating) = 1 / (1 + exp[-(-4.825 + 0.8722 * account.balance + ... + 1.449 * foreign.worker)])

# 예측
library(caret)
pred <- predict(step_lr_credit, test[,-1], type="response")  # test 데이터에서 종속변수 빼고 넣기, 확률값 산출(response)
pred1 <- as.data.frame(pred)
pred1$grade <- ifelse(pred1$pred < 0.5, 0, 1) # 확률값 활용해 범주 예측

# 오분류표(factor)
confusionMatrix(as.factor(pred1$grade), test[,1], positive='1') # Accuracy : 0.7667 / Sensitivity : 0.9024 / Specificity : 0.4737

# ROC(numeric)
library(ROCR)
pred_step_lr_credit <- prediction(as.numeric(pred1$grade), as.numeric(test[,1]))
plot(performance(pred_step_lr_credit, "tpr", "fpr")) # ROC
abline(a=0, b=1, lty=2) # baseline
performance(pred_step_lr_credit, "auc")@y.values # AUROC : 0.6880616


## example(다항로지스틱회귀분석 iris train : test = 7:3)

# 분할
idx <- sample(1:nrow(iris), 0.7*nrow(iris), replace=FALSE)
train <- iris[idx,]
test <- iris[-idx,]

# 모형 적합
library(nnet)
multi_lr_iris <- multinom(Species ~ ., data= train)

# 예측
pred <- predict(multi_lr_iris, test[,-5])

# 오분류표
library(caret)
confusionMatrix(pred, test[,5]) # Accuracy 1 / class 별 타 특이도, 민감도 


## 2. 의사결정나무
##    의사결정 규칙으로 이뤄진 나무 모형
##    분류나무와 회귀나무 존재

##    과정
##    성장 -> 가지치기 -> 타당성 평가 -> 해석 및 예측
##    1) 성장 
##       각 마디 별 최적의 분리규칙 찾다가 정지규칙 만족 시 중단
##       분리규칙을 정하는 분리 기준은 종속변수의 이산형/연속형 에 따라 다름
##       이산형 종속변수 -> 카이제곱 통계량 p값 / 지니지수 / 엔트로피 지수
##       연속형 종속변수 -> 분산분석의 F통계량 / 분산 감소량
##       정지규칙은 모형의 깊이나 끝마디 최소 레코드 수로 결정됨
##    2) 가지치기
##       일반적으로 마디에 속하는 자료가 일정 수 이하일 때 분할 정지하고 비용-복잡도 가지치기 이용해 가지치기함
##    3) 타당성 평가 단계
##       이익도표, 위험도표, test 데이터 결과로 모형 결과 평가
##    4) 해석 및 예측
##       구축된 모형 해석 및 예측

##    의사결정나무 알고리즘
##    1) CART(Classification and Regression Tree)
##       불순도 측도로 종속 변수가 범주형인 경우 지니지수, 연속형인 경우 분산 이용한 이진분리
##       개별 입력변수 뿐 아니라 입력변수들의 선형결합 중 최적 분리 탐색 가능
##    2) C4.5와 C5.0
##       CART와 다르게 다지분리 가능, 범주형 독립변수에 대해 범주 수만큼 분리
##       불순도 측도로는 엔트로피 지수
##    3) CHAID(CHi-squared Automatic Interaction Detection)
##       가지치기 하지 않고 적당한 크기에서 모형 성장 중지(독립변수 반드시 범주형)
##       불순도 측도로 카이제곱 통계량

##    R code
##    rpart(formula, data, method, control=rpart.control(), ...) method : "anova", "poisson", "class", "exp" / control : 모형 option
##    example(credit train:test = 7:3)
credit <- read.csv('C:/cha/기타/R/credit_final.csv', header=TRUE, sep=',')
credit$credit.rating <- as.factor(credit$credit.rating)

# 분할
set.seed(123)
idx <- sample(1:nrow(credit), 0.7*nrow(credit), replace=F)
train <- credit[idx,]
test <- credit[-idx,]

# 모형 적합
library(rpart)
library(rpart.plot)
dt_credit <- rpart(credit.rating~.,
                    method = "class",
                    data = train,
                    control = rpart.control(maxdepth=5,
                                            minsplit=15))

# plot 확인
prp(dt_credit, type = 4, extra = 2)

# 최적 나무 선정(cptable에서 xerror가 가장 낮은 split 개수 선택)
dt_credit$cptable # nsplit : 분할횟수 / xerror : 해당 CP의 CV했을 때 오류율 / xstd : 해당 CP에서 CV했을 때 편차
opt <- which.min(dt_credit$cptable[,"xerror"])
cp <- dt_cred$cptable[opt,"CP"]
plotcp(dt_credit) # 나무의 크기가 6일 때 최적의 나무(split 5)

# 예측
library(caret)
pred_dt <- 
