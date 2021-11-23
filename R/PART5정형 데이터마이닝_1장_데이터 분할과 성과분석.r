# 윤종식, 『ADP 실기 데이터 분석 전문가』, (주)데이터에듀-2020, 236-243

# 데이터 분할과 성과분석


## 1. 데이터 분할
##    모형 개발 시 훈련(trian), 검증(validation), 시험(test)용 데이터로 분할해 사용

##   R code
##   sample(x, size, replace=FALSE,prob,...) : 무작위 표본 추출
##   caret::createDataPartition(y, times, p, list=TRUE,...) : 종속변수 값의 비율이 원본데이터와 같게 유지  times : 생성할 분할의 수 / p : 훈련데이터에 사용할 데이터 비율 / list : 결과 list로 반환 여부
##   example(train:validataion:test = 5:3:2)
credit_df <- read.csv('C:/cha/기타/R/german_credit_dataset.csv', header=TRUE, sep=',')
nrow(credit_df) # 1000 rows
idx <- 1 :nrow(credit_df) # 원데이터 index 저장

test_idx <- sample(idx, 0.2 * nrow(credit_df)) # test index 추출
idx <- setdiff(idx, test_idx) # 원데이터 index에서 test index 제거

train_idx <- sample(idx, replace = FALSE, 0.5 * nrow(credit_df)) # test index 제거된 index에서 train index 추출

validation_idx <- setdiff(idx, train_idx) # train index 제외한 나머지 index는 validation index


test <- credit_df[test_idx,] # 200
train <- credit_df[train_idx,] # 500
validation <- credit_df[validation_idx,] #300

##    example(createDataPartition)
library(caret)
parts <-caret::createDataPartition(credit_df$credit.rating, # 종속변수 지정
                                   times=1, # 생성할 데이터 분할은 1개로 지정
                                   p = 0.7) # 훈련데이터 70%로
parts<-as.vector(parts$Resample1)
train <- credit_df[parts,]
test <- credit_df[-parts,]

nrow(train) # 700
nrow(test) # 300

table(train$credit.rating)  # 0 : 208 / 1 : 492 약 30%
table(test$credit.rating)   # 0 : 92  / 1 : 208 약 30%


## 2. 성과분석
##    검증용 데이터로 모형의 분류 및 예측 정확도를 평가한다.
##    분류모형에서는 오분류표(Confusion Matrix), ROC그래프 등으로 평가한다.

##    오분류표
##    종속변수의 실제 범주와 모형에 의해 예측된 것 사이의 관계를 표로 나타낸 것
##    TP(True Positive) : 예측을 Positive로 했을 때 실제도 Positive여서 맞춘 경우
##    TN(True Negative) : 예측을 Negative로 했을 때 실제도 Negative여서 맞춘 경우
##    FP(FALSE Positive) : 예측을 Positive로 했을 때 실제는 Negative여서 틀린 경우
##    FN(FALSE Negative) : 예측을 Negative로 했을 때 실제는 Positive여서 틀린 경우

##    분석 지표
##    정분류율(Accuracy) = (TN + TP) / (TN + TP + FN + FP) : 전체에서 실제값과 예측치가 일치하는 정도
##    오분류율(Error Rate) = 1- Accuracy : 전체에서 실제값과 예측치가 다른 정도
##    민감도(Sensitivity) = TP / (TP + FN) : 실제값이 Positive인 것 중 예측이 일치하는 정도
##    특이도(Specificity) = TN / (TN + FP) : 실제값이 Negative인 것 중 예측이 일치하는 정도
##    정밀도(Precision) = TP / (TP + FP) : Positive로 예측한 것 중 실제값과 일치하는 정도
##    재현율(Recall)) = TP / (TP + FN) : 실제값이 Positive인 것 중 예측이 일치하는 정도 (= 민감도(Sensitivity))
##    F1-score = 2/(1/Precision + 1/Recall) : 정밀도와 재현율을 보정해 하나의 지표로 나타낸 값  

##    ROC그래프
##    x축=1-특이도(FP ratio), y축=민감도(TP ratio)
##    ROC그래프의 아래 면적(AUROC)이 클수록 좋은 모형

##    R code
##    Confusion Matrix
##    caret::confusionMatrix(data, reference) data : 예측값 / referene : 실제값
##    ROC
##    prediction(predictions, labels)  prediction : 예측값 / labels : 실제값 ** 두 값 모두 numeric이어야 함!!!!!!!
##    performance(prediction.object, acc(accuracy), fpr(FP Rate), tpr(TP Rate),...)  # prediction : prediction 객체 / 해당 함수로 ROC그리기 + AUC 계산


##    example(Confusion Matrix)
library(caret)
predicted <- factor(c(1,0,1,1,0,0,0,1,1,0,0,1))
actual <- factor(c(1,0,0,1,0,1,1,1,0,0,1,1))
caret::confusionMatrix(actual, predicted) # Accuracy : 0.5833 / Sensitivity : 0.5 / Specificity : 0.6667

##    example(ROC)
library(ROCR)
pred <- prediction(as.numeric(predicted), as.numeric(actual)) # prediction 객체 생성
plot(performance(pred, "tpr","fpr")) # ROC
performance(pred, "auc")@y.values # AUROC = 0.5857143
