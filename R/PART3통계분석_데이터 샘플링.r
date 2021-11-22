# 데이터 샘플링


## 용어 정리
### 모집단(population) : 조사하고자하는 대상 집단 전체
### 원소(element) : 모집단을 구성하는 개체
### 표본(sample) : 조사하기 위해 추출한 모집단의 일부 원소
### 모수(parameter) : 표본 관측에 의해 구하고자 하는 모집단에 대한 정보

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
##    ex. 우리나라 시도를 무작위 선택 후 선택된 시도에서 군과 구를 무작위 선정, 다시 읍면동 선정한 후 해당 읍면동에 속한 주민등록번호를 무작위로 선택해 표본 추출

## R을 이용한 표본 추출
## 1.단순 임의 추출법(simple random sampling)
##   sample(x, size, replace=FALSE, prop=NULL)  x : 표본 추출 벡터 / size : 표본 크기 / replace : 복원추출여부 / prob : 데이터 뽑을 때 가중치 지정
##   example (iris 데이터 7:3 추출)
print("hi")
train_idx <- sample(c(1:nrow(iris)), size = 0.7 * nrow(iris), replace=FALSE) 
train_df <- iris[train_idx,]
test_df <- iris[-train_idx,]











