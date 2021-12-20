# https://loklee9.tistory.com/212




# SOM


## 1) 정의
##    비지도학습 신경망으로 고차원 데이터를 이해하기 위해 저차원 뉴런으로 정렬해 지도(map) 형태로 형상화 하는 기법

## 2) code example
##    iris 데이터를 som이용해 군집화
library(kohonen)

idx <- sample(1:nrow(iris), 100, replace=F)
train <- list( x = as.matrix(iris[idx,-5]), Species = as.factor(iris[idx,5]))
test <- list( x = as.matrix(iris[-idx,-5]), Species = as.factor(iris[-idx,5]))
# SOM input은 list

# somgrid함수 이용해 경쟁층 구현
gr <- somgrid(xdim = 3, ydim = 5, topo = "hexagonal")

# 학습
ss <- supersom(train, gr, rlen = 200, alpha = c(0.05,0.01))
# rlen : 학습횟수
# alpha : learning rate로 0.05로 시작해 0.01까지 학습

summary(ss)

# 군집화잘 되었는지 시각화하면서 확인
# 학습이 진행될수록 경쟁층 뉴런과 입력 데이터 거리 짧아짐을 확인
plot(ss, type="changes")

# som 모델의 각 뉴런이 몇 개의 입력 데이터와 맵핑되는지 확인
plot(ss, type="count", main="Node Counts")


# 경쟁층 뉴런에 속한 입력층의 친구들(Species)이 누군지 확인
plot(ss, type="codes")

# 예측
test.pred <- predict(ss, test)
test.pred$predictions$Species
table(test.pred$predictions$Species, test$Species)
