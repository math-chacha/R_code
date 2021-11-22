# 윤종식, 『ADP 실기 데이터 분석 전문가』, (주)데이터에듀-2020, 136-142

# 데이터 샘플링


## 표본추출 방법
## 1. 단순 임의 추출법(simple random sampling)
##    각 원소에 번호를 부여해 랜덤하게 n개를 추출하는 방법으로 모든 원소는 선택될 확률 동일 / 복원&비복원추출로 나뉘어짐
## 2. 계통추출법(systematic sampling)
##    랜덤하게 정렬된 표본에서 시간 혹은 공간적으로 일정한 간격을 두고 표본을 추출하는 방법 / 임의의 위치에서 매 k번째 원소를 추출하는 방법
## 3. 집락추출법(cluster random sampling)
##    군집을 구분하고 특정 군집을 먼저 선택한 후 해당 군집에서만 표본을 추출하는 방법
## 4. 층화추출법(stratified random sampling)
##    모집단이 이질적인 몇 개의 계층으로 이루어져 있을 때 모든 계층으로부터 원소를 임의로 추출하여 각 계층을 고루 대표할 수 있도록 랜덤하게 표본을 추출하는 방법
## 5. 다단계 추출(multi-stage sampling)
##    표본 추출 과정을 여러 단계로 나누어서 진행하는 것으로, 표본으로 추출된 집단 내에서 다시 일부를 뽑고 그 집단에서 다시 일부를 뽑는 방법

## R을 이용한 표본 추출
## 1.단순 임의 추출
##   sample(x, size, replace=FALSE, prop=NULL)  x : 표본 추출 벡터 / size : 표본 크기 / replace : 복원추출여부 / prob : 데이터 뽑을 때 가중치 지정
##   example (iris 데이터 6:4 추출)
train_idx <- sample(c(1:nrow(iris)), size = 0.6 * nrow(iris), replace=FALSE) 
train_df <- iris[train_idx,]
test_df <- iris[-train_idx,]
cat(nrow(iris),nrow(train_df),nrow(test_df)) # 150, 90, 60

## 2. 층화 임의 추출
##    sampling::strata(data, stratanames=NULL,size, method = c("srswor","srswr","poisson","systematic"), pik, description=FALSE)
##    stratanames : 계층 구분 변수 / method : 차례로 비복원단순임의추출, 복원단순임의추출, 포아송추출, 계통추출 / pik : 데이터를 표본에 포함시킬 확률 / description : 표본크기와 모집단 크기 추출할지 여부
##    getdata(data,m) m : 추출된 벡터 혹은 데이터 프레임
##    strata 함수 통해 층화 임의 추출한 데이터는 getdata함수 통해 확인 가능
##    example (iris에서 Species별 10,5,5 층화임의추출)
library(sampling)
sample <- sampling::strata(data = iris, stratanames=c("Species"), size = c(10,5,5), method="srswor")
iris_sample <- getdata(iris, sample)
table(iris_sample$Species) # setosa : 10 / versicolor : 5 / virginica : 5



