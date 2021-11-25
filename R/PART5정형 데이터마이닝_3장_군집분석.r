# 윤종식, 『ADP 실기 데이터 분석 전문가』, (주)데이터에듀-2020, 300-317


# 군집분석

## 1. 군집분석이란
##    각 객체의 유사성을 측정해 유사성이 높은 대상을 집단으로 분류하고 집단 간 상이성을 찾는 분석
##    종속변수(반응변수)가 필요없는 비지도학습
##    계층적 군집 / 분할적 군집
##    계층적 군집 - 합병형 : 단일(최단)연결법 / 완전(최장)연결법 / 평균연결법 / 중심연결법 / 와드연결법 
##               - 분리형 : 다이아나 방법
##    분할적 군집 - 프로토타입 : k중심군집 / k평균군집 / k중앙값군집 / k-medoid군집 / 퍼지군집
##               - 분포기반 : 혼합분포군집
##               - 밀도기반 : 중심밀도군집 / 밀도기반군집
##    거리함수
##    연속형 거리함수 : 유클리드거리, 표준화거리, 마할라노비스 거리, 체비셰프 거리, 맨하탄 거리, 캔버라 거리, 민코우스키 거리 등
##    이산형 거리함수 : 자카드 거리, 코사인 유사도 등


## 2. 계층적 군집분석
##    n개 군집으로 시작해 점점 군집 개수를 줄여가는 방법
##    R code
##    dist(data, method)  method : "euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski"
##    hclust(data, method)  method : "single", "complete", "average", "median", " ward.D"
##    example(USArrests data)
us_data <- USArrests
us_dist <- dist(us_data,"euclidean")
us_dist
# 계층적 군집분석
us_hclust_single <- hclust(us_dist, method="single") # 최단연결법
plot(us_hclust_single)
us_hclust_complete <- hclust(us_dist, method="complete") # 최장연결법
plot(us_hclust_complete)
us_hclust_average <- hclust(us_dist, method="average") # 평균연결법
plot(us_hclust_average)
# group 나누기1
group <- cutree(us_hclust_average, k=6) # 평균연결법 결과를 6개의 그룹으로 나누기
group
# group 나누기2
plot(us_hclust_average)
rect.hclust(us_hclust_average, k=6, border="red") # 시각화 결과를 나누기


## 3. 비계층적 군집분석
##    k평균 군집분석
##    장점 :
##           1) 알고리즘 단순, 빠르게 수행
##           2) 계층적 군집분석에 비해 많은 양 데이터 다룰 수 있음
##           3) 다양한 형태의 데이터에 적용 가능
##           4) 내부 구조에 대한 사전정보 없이 의미 있는 자료 구조 탐색 가능
##    단점 :
##           1) 군집 수, 가중치, 거리 정의 어려움
##           2) 결과 해석 어려움
##           3) 잡음, 이상치에 영향 큼
##           4) 초기 군집 수 결정 어려움

##    R code
##    kmeans(data, centers, ...) centers : 군집의 수
##    NbClust::NbClust(data, min.nc, max.nc, method,...) min.nc : 최소 군집 수 / method : "kmeans", "median", "single", "complete", "average" 등
##    example(credit trian:test = 7:3)
##    example(credit train:test = 7:3 분할 + 모형 적합)
credit <- read.csv('C:/cha/기타/R/credit_final.csv', header=TRUE, sep=',')
credit$credit.rating <- as.factor(credit$credit.rating)

# 분할
set.seed(123)
idx <- sample(1:nrow(credit), 0.7*nrow(credit), replace=F)
train <- credit[idx,]

# 군집분석
kmeans_credit <- kmeans(train[,-1], centers = 2) # 학습데이터를 2개의 그룹으로 kmeans
kmeans_credit # Within cluster sum of squares by cluster = 70.2%

# 결과
kmeans_table <- table(train$credit.rating, kmeans_credit$cluster)
kmeans_table 
(kmeans_table[1,1] + kmeans_table[2,2]) / sum(kmeans_table) # Accuracy : 0.6685714


##    example(최적 군집 탐색 NbClust)
library(NbClust)
nc <- NbClust(train[,-1], min.nc = 2, max.nc = 15, method = "kmeans") # the best number of clusters is  2


## 4. 혼합 분포 군집
##    모형 기반 군집 방법이며, 데이터가 k개의 모수적 모형의 가중합으로 표현되는 모집단 모형으로부터 나왔다는 가정하에서 모수와 함께 가중치를 자료로부터 추정하는 방법
##    일반적으로 혼합모형에서의 모수와 가중치의 추정에는 EM알고리즘 사용
##    데이터가 커지면 수렴 시간 오래 걸림 / 데이터가 적으면 추정 정도 떨어짐
##    이상치에 민감

##    R code
##    Mclust(data, G, ...)  G : BIC 계산 시 클러스터 수
##    example(iris)
library(mclust)
mc <- Mclust(iris[,-5], G=3)  # iris의 Species는 3종이므로 클러스터 수를 3으로 지정
summary(mc, parameter=T) # 모형 요약 결과 - 클러스터링 결과, 클러스터링 중심 등

plot.Mclust(mc, what="classification") # clustering 결과 확인