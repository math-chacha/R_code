# 윤종식, 『ADP 실기 데이터 분석 전문가』, (주)데이터에듀-2020, 275-299

# 분류분석
# 활용 데이터 : credit 은행 자금 대출 신청한 1000명의 자산 신용 관련 데이터 (credit_final.csv)
# 종속변수 : credit.rating 0 : 불량 / 1: 우량
# 독립변수
# 수치형 : 4개
# 범주형 : 18개


## 4. SVM(Support Vector Machine)
##    머신러닝 기법 중 하나로 패턴인식, 자료 분석등을 위한 지도학습 모델
##    판별 경계선(초평면)을 통해 분류하는 모형
##    각 관측치에서부터 거리가 가장 먼 초평면을 판별 경계선으로 결정
##    kernel 조정을 통해 비선형 모형도 적합 가능
##    용어 정리
##    Hyperplane(초평면) : n차원 공간에서 한 차원 낮은 n-1차원 공간
##    Support Vector(서포트 벡터) : 초평면에 가장 가까이에 붙어있는 최전방 데이터들
##    Margin(마진) : Support Vector와 초평면 사이의 수직거리

##    R code
##    e1071::svm(formula, data, kernel, gamma, cost, ...)  kernel : "radial"/"linear"/"polynomial"/"sigmoid", gamma : 초평면의 기울기, cost : 과적합 막는 정도
##    e1071::tune.svm(formula, data, kernel, gamma, cost, ...)  : 최적 파라미터 결정 위한 함수
##    example(credit train:test = 7:3 분할 + 모형 적합)
credit <- read.csv('C:/cha/기타/R/credit_final.csv', header=TRUE, sep=',')
credit$credit.rating <- as.factor(credit$credit.rating)

# 분할
set.seed(123)
idx <- sample(1:nrow(credit), 0.7*nrow(credit), replace=F)
train <- credit[idx,]
test <- credit[-idx,]

# 모형 적합
library(e1071)
#####  최적 파라미터 확인
tune.svm(credit.rating~.,
         data = credit,    # 전체 데이터에 대해 각 파라미터 최적값 찾는 것이 중요
         gamma = 10^(-6:-1),
         cost = 10^(1:2))  # gamma 6개 *  cost 2개 = 12개 파라미터 조합 확인, kernel default = radial(가우시안 RBF)
                           # gamma=0.01 / cost = 10 : 최적 파라미터

##### 최적 파라미터 활용한 모형 적합
svm_credit <- svm(credit.rating~.,
                  data = train,
                  gamma = 0.01,
                  cost = 10)

summary(svm_credit) # Number of Support Vector 확인 가능

# 예측
library(caret)
pred_svm <- predict(svm_credit, test[,-1], type="class")

# 오분류표
confusionMatrix(pred_svm, test[,1]) # Accuracy : 0.7533 / Sensitivity : 0.5474 / Specificity : 0.8488

# ROC
library(ROCR)
prediction_svm <- prediction(as.numeric(pred_svm), as.numeric(test[,1]))
plot(performance(prediction_svm, "tpr", "fpr")) # ROC
performance(prediction_svm, "auc")@y.values # AUROC : 0.6980745

##    example(다항SVM iris train : test = 7:3)
# 분할
idx <- sample(1:nrow(iris), 0.7*nrow(iris), replace=FALSE)
train <- iris[idx,]
test <- iris[-idx,]

# 모형 적합
library(e1071)
tune.svm(Species ~ .,
         data = iris,
         gamma = 2^(-1:1),
         cost = 2^2:4) # gamma = 0.5 / cost = 4 최적 파라미터

multi_svm_iris <- svm(Species ~ .,
                      data = iris,
                      gamma = 0.5,
                      cost = 4)
summary(multi_svm_iris)


# 예측
pred <- predict(multi_svm_iris, test[,-5])

# 오분류표
library(caret)
confusionMatrix(pred, test[,5]) # Accuracy 0.9556 / class 별 타 특이도, 민감도 


## 5. 나이브 베이즈 분류(Naive Bayes Classification)
##    머신러닝의 일종으로 특성들 사이의 독립을 가정하는 베이즈 정리를 적용한 확률 분류기
##    베이즈 정리 : 어떤 사건이 서로 배반하는 원인 둘에 의해 일어난다고 할 때 실제 사건이 일어났을 때 이것이 두 원인 중 하나일 확률을 구하는 정리
##                 P(B|A) = P(A|B)P(B) / P(A)

##    R code
##    e1071::naiveBayes(formula, data, laplace=0, ...) laplace : 라플라스 보정 여부
##    example(credit train:test = 7:3 분할 + 모형 적합)
credit <- read.csv('C:/cha/기타/R/credit_final.csv', header=TRUE, sep=',')
credit$credit.rating <- as.factor(credit$credit.rating)

# 분할
set.seed(123)
idx <- sample(1:nrow(credit), 0.7*nrow(credit), replace=F)
train <- credit[idx,]
test <- credit[-idx,]

# 모형 적합
library(e1071)
nb_credit <- naiveBayes(credit.rating~.,
                        data = train,
                        laplace = 0)
nb_credit # A-priori probabilities : 사전 확률 / Conditional probabilities : 각 변수 별 조건부 확률(수치형 : 평균, 표준편차 제공)


# 예측
library(caret)
pred_nb <- predict(nb_credit, test[,-1], type="class")

# 오분류표
confusionMatrix(pred_nb, test[,1]) # Accuracy : 0.7233 / Sensitivity : 0.6842 / Specificity : 0.7415

# ROC
library(ROCR)
prediction_nb <- prediction(as.numeric(pred_nb), as.numeric(test[,1]))
plot(performance(prediction_nb, "tpr", "fpr")) # ROC
performance(prediction_nb, "auc")@y.values # AUROC : 0.712837


## 6. K-NN(K-Nearest Neighbor)
##    새로운 데이터의 클래스를 해당 데이터와 가장 가까운 k개 데이터들의 범주로 결정
##    여러 가지 거리 사용 가능(유클리디안, 맨하탄, 민코우스키, 코사인 등)
##    K는 일반적으로 train 데이터 개수의 제곱근으로 설정하며 홀수로 설정하는 것이 좋다

##    R code
##    class::knn(train, test, cl, k,...)  cl : 훈련 데이터의 종속변수 / k : 이웃의 수
##    example(credit train:test = 7:3 분할 + 모형 적합)
credit <- read.csv('C:/cha/기타/R/credit_final.csv', header=TRUE, sep=',')
credit$credit.rating <- as.factor(credit$credit.rating)

# 분할
set.seed(123)
idx <- sample(1:nrow(credit), 0.7*nrow(credit), replace=F)
train <- credit[idx,]
test <- credit[-idx,]

# 모형 적합
library(class)
##### knn 모형 생성 함수 생성
knn_model_create <- function (train, test, class, k){
                                                    model <- knn (train = train, # without label
                                                                    test = test, # without label
                                                                    cl = class, # only train label
                                                                    k = k)
                                                    return (model)
                                                    }
##### knn 모형 평가 지표(정분류율) 확인 함수 생성
knn_accuracy <- function(model, actual){
                                        result_table <- table(model, actual)  # actual : only test label
                                        accuracy <- (result_table[1,1] + result_table[2,2]) / sum(result_table)
                                        
                                        return(accuracy)
                                       }


knn3_credit <- knn_model_create(train = train[,-1], test = test[,-1], class = train[,1], k = 3)
knn7_credit <- knn_model_create(train = train[,-1], test = test[,-1], class = train[,1], k = 7)
knn10_credit <- knn_model_create(train = train[,-1], test = test[,-1], class = train[,1], k = 10)

knn_accuracy(knn3_credit, test[,1]) # 0.603333
knn_accuracy(knn7_credit, test[,1]) # 0.656666
knn_accuracy(knn10_credit, test[,1]) # 0.676666

##### k 변경해가며 정분류율 확인
result <- numeric()
k_list <- 3:22 

for (i in k_list){
    knn_credit <- knn_model_create(train = train[,-1], test = test[,-1], class = train[,1], k = i)
    result[i-2] <- knn_accuracy(knn_credit, test[,1])
}
sort(result, decreasing=T) # 0.6900000 최고
which(result==max(result)) # k가 15 17 20일 때 가장 좋은 성능 69%


## 7. 인공신경망(Artificial Neural Network)
##    동물 뇌신경계를 모방한 모형으로 가중치를 반복적으로 조정하며 학습
##    구조
##    input - layer_1 - layer_2 - ... - layer_n - output
##    input : row 하나의 column별 값
##    layer 와 연결된 가중치들로 가중합 계산 후 activation function(ex. Relu, Sigmoid, softmax 등) 적용 한 것이 각 layer의 output이 됨

##    R code
##    library(nnet) : 전통적 역전파 가지고 신경망 훈련
##    nnet::nnet(formula, data, size, maxit, decay=5e-04,...) maxit : 학습 반복 횟수
##    NeuralNetTools::garson(mod_in) 변수 중요도 파악 ,, mod_in : 생성된 인공신경만 모델
##    library(neuralnet) : 탄력적 역전파 사용해 빠른 인공신경망 알고리즘
##    neuralnet:neuralnet(formula, data, algorithm, threshold, hidden, stepmax, ...)

##    example(nnet credit train:test = 7:3 분할 + 모형 적합)
credit <- read.csv('C:/cha/기타/R/credit_final.csv', header=TRUE, sep=',')
credit$credit.rating <- as.factor(credit$credit.rating)

# 분할
set.seed(123)
idx <- sample(1:nrow(credit), 0.7*nrow(credit), replace=F)
train <- credit[idx,]
test <- credit[-idx,]

# 모형 적합
library(nnet)
nn_credit <- nnet(credit.rating~.,
                        data = train,
                        size = 2,
                        maxit = 200,
                        decay = 5e-04)

summary(nn_credit) # 가중치 확인 가능 a 20-2-1 : 20개 입력노드, 2개 layer, 1개 출력노드

# 가중치 시각화
library(devtools)
source_url('https://gist.githubusercontent.com/Peque/41a9e20d6687f2f3108d/raw/85e14f3a292e126f1454864427e3a189c2fe33f3/nnet_plot_update.r')

X11()
plot.nnet(nn_credit)  #아래와 같은 그래프가 나타나며, summary 결과의 갯수와 동일한 갯수로 나타남.

#변수 중요도 파악
library(NeuralNetTools)

X11()
garson(nn_credit)


# 예측
library(caret)
pred_nn <- predict(nn_credit, test[,-1], type="class")

# 오분류표
confusionMatrix(as.factor(pred_nn), test[,1], positive="1") # Accuracy : 0.6833 / Sensitivity : 0.000 / Specificity : 1.000

# ROC
library(ROCR)
prediction_nn <- prediction(as.numeric(pred_nn), as.numeric(test[,1]))
plot(performance(prediction_nn, "tpr", "fpr")) # ROC
performance(prediction_nn, "auc")@y.values # AUROC : 0.5 왜 이러지,,,


##    example(neuralnet infert 자연유산%인공유산 후 불임 데이터)
##    8개 변수(label 포함) & 248row
# 분할
data(infert)
in.part <- createDataPartition(infert$case,
                               times = 1,
                               p = 0.7)
table(infert[in.part$Resample1,"case"]) # 0 : 117 / 1 : 57

parts <- as.vector(in.part$Resample1)
train.infert <- infert[parts,]
test.infert <- infert[-parts,]

# 모형 적합
library(neuralnet)

neuraln_credit <- neuralnet(case ~ age + parity + induced + spontaneous,
                            data= train.infert,
                            hidden = c(2,2),
                            algorithm = "rprop+",
                            threshold = 0.01,
                            stepmax = 1e+5)
plot(neuraln_credit)
names(neuraln_credit)


# 예측
test.infert$nn.model2_pred.prob <- compute(neuraln_credit, covariate=test.infert[,c(2:4,6)])$net.result # compute 함수 사용 / covariate : 모형 사용 변수
test.infert$nn.model2_pred <- ifelse(test.infert$nn.model2_pred.prob > 0.5, 1, 0)

# 오분류표
confusionMatrix(as.factor(test.infert$nn.model2_pred), as.factor(test.infert[,"case"])) # Accuracy : 0.6486 / Sensitivity : 0.7083 / Specificity : 0.5385


# ROC
library(ROCR)
prediction_neuraln <- ROCR::prediction(as.numeric(test.infert$nn.model2_pred), as.numeric(test.infert$case))

as.numeric(test.infert$nn.model2_pred)
as.numeric(test.infert$case)
X11()
plot(performance(prediction_neuraln, "tpr", "fpr")) # ROC
performance(prediction_neuraln, "auc")@y.values # AUROC : 0.6233974