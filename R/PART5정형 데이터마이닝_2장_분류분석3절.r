# 윤종식, 『ADP 실기 데이터 분석 전문가』, (주)데이터에듀-2020, 261-274

# 분류분석
# 활용 데이터 : credit 은행 자금 대출 신청한 1000명의 자산 신용 관련 데이터 (credit_final.csv)
# 종속변수 : credit.rating 0 : 불량 / 1: 우량
# 독립변수
# 수치형 : 4개
# 범주형 : 18개


## 3. 앙상블(ensemble) 기법
##    여러 개의 예측모형들을 만든 후 예측모형들을 조합해 하나의 최종 예측모형을 만드는 기법
##    배깅 / 부스팅 / 랜덤포레스트 등

##    1) 배깅
##       데이터에서 여러 개의 붓스트랩(bootstrap) 자료 생성해 예측 모형 만든 후 결합해 최종 예측 모형 생성
##       보팅(voting) : 여러 모형 중 다수결에 의해 최종 결과 선정
##       배깅은 가지치기 하지 않고 최대로 성장시켜 사용(가지치기 과정 없어도 됨)

##       R code
##       adabag::bagging(formula, data, mfinal, control=, ...)
##       example(credit train:test = 7:3 분할 + 모형 적합)
credit <- read.csv('C:/cha/기타/R/credit_final.csv', header=TRUE, sep=',')
credit$credit.rating <- as.factor(credit$credit.rating)

# 분할
set.seed(123)
idx <- sample(1:nrow(credit), 0.7*nrow(credit), replace=F)
train <- credit[idx,]
test <- credit[-idx,]

# 모형 적합
library(adabag)
bag_credit <- bagging(credit.rating~.,
                      data=train,
                      mfinal=15)

names(bag_credit) # bag_credit 내 함수 : formula / trees / votes / prob / class / samples / importance / terms / call

bag_credit$importance # 변수 중요도 확인 가능(account.balance-credit.amount-credit.duration.months 순)

# 예측
library(caret)
pred_bag <- predict(bag_credit, test[,-1], type="class")

# 오분류표
confusionMatrix(as.factor(pred_bag$class), test[,1]) # Accuracy : 0.7433 / Sensitivity : 0.4526 / Specificity : 0.8780

# ROC
library(ROCR)
prediction_bag <- prediction(as.numeric(pred_bag$class), as.numeric(test[,1]))
plot(performance(prediction_bag, "tpr", "fpr")) # ROC
performance(prediction_bag, "auc")@y.values # AUROC : 0.6653402


## 2. 부스팅
##    여러 개의 알고리즘이 순차적으로 학습-예측을 하면서 이전에 학습한 알고리즘의 예측이 틀린 데이터를 올바르게 예측할 수 있도록, 다음 알고리즘에, 가중치를 부여하여 학습과 예측하는 기법
##    AdaBoost / GBM(Gradient Boosting Machine) / XGBoost / LightBoost

##    R code
##    adabag::boosting(formula, data, boos=T/F, mfinal, control=,...)
##       example(credit train:test = 7:3 분할 + 모형 적합)
credit <- read.csv('C:/cha/기타/R/credit_final.csv', header=TRUE, sep=',')
credit$credit.rating <- as.factor(credit$credit.rating)

# 분할
set.seed(123)
idx <- sample(1:nrow(credit), 0.7*nrow(credit), replace=F)
train <- credit[idx,]
test <- credit[-idx,]

# 모형 적합
library(adabag)
boost_credit <- boosting(credit.rating~.,
                      data=train,
                      boos=T,
                      mfinal=80)

names(boost_credit) # boost_credit 내 함수 : formula / trees / weights / votes / prob / class / importance / terms / call

boost_credit$importance # 변수 중요도 확인 가능(credit.amount-age-credit.duration.months 순)

# 예측
library(caret)
pred_boost <- predict(boost_credit, test[,-1], type="class")

# 오분류표
confusionMatrix(as.factor(pred_boost$class), test[,1]) # Accuracy : 0.7167 / Sensitivity : 0.4947 / Specificity : 0.8195

# ROC
library(ROCR)
prediction_boost <- prediction(as.numeric(pred_boost$class), as.numeric(test[,1]))
plot(performance(prediction_boost, "tpr", "fpr")) # ROC
performance(prediction_boost, "auc")@y.values # AUROC : 0.6571245


## 3. 랜덤포레스트
##    의사결정나무가 분산이 크다는 점 고려해 더 많은 무작위성을 통해 약한 학습기 생성 후 이를 선형 결합해 최종 학습기를 만드는 방법
##    해석 어렵다는 단점 있지만 변수 제거 없이 모형 적합해 정확도 좋음

##    R code
##    randomForest::randomForest(formula, data, ntree, mtry, ...)
##       example(credit train:test = 7:3 분할 + 모형 적합)
credit <- read.csv('C:/cha/기타/R/credit_final.csv', header=TRUE, sep=',')
credit$credit.rating <- as.factor(credit$credit.rating)

# 분할
set.seed(123)
idx <- sample(1:nrow(credit), 0.7*nrow(credit), replace=F)
train <- credit[idx,]
test <- credit[-idx,]

# 모형 적합
library(randomForest)
rf_credit <- randomForest(credit.rating~.,
                          data=train,
                          ntree=50,
                          mtry=sqrt(20),
                          importance=T)

names(rf_credit) # rf_credit 내 함수 : formula / trees / weights / votes / prob / classes / err.rate / predicted ...

varImpPlot(rf_credit) # 변수 중요도 확인 가능


# 예측
library(caret)
pred_rf <- predict(rf_credit, test[,-1], type="class")

# 오분류표
confusionMatrix(pred_rf, test[,1]) # Accuracy : 0.7467 / Sensitivity : 0.4421 / Specificity : 0.8878

# ROC
library(ROCR)
prediction_rf <- prediction(as.numeric(pred_rf), as.numeric(test[,1]))
plot(performance(prediction_rf, "tpr", "fpr")) # ROC
performance(prediction_rf, "auc")@y.values # AUROC : 0.6649551
