# https://rpubs.com/yoompubs/467234


# OVERSAMPLING


## 1. 정의
##    imbalance한 데이터는 학습이 어려울 확률이 높음. 이를 해결하기 위해 나온 기법
##    어렵다는 것은 분류기(classifier)는 분류 오류(classification error)를 줄이는 것을 목적으로 학습하는데 
##    imbalance가 심하면(majority class가 월등히 많으면) 분류기는 majority class로 예측해버리는 경향이 생긴다는 것

## 2. code example
##    data : newthyroid1 
##           환자들의 갑상선 기능 항진증(hyperthyroidism)을 예측하는 데이터
library(imbalance)
head(newthyroid1)
table(newthyroid1$Class) # negative 180 / positive 35

# 불균형비율 ( size of minority class / size of majority class)
imbalanceRatio(newthyroid1) # 35/180

# 1) MWMOTE(Majority Weighted Minority Oversampling TEchinique)
#    SMOTE의 개선된 버젼으로 두 클래스의 경계에 있는 데이터에 가중치를 주고 minority clusters의 사이즈를 줄이며 경계 데이터를 추가
mwmote_result <- mwmote(newthyroid1, numInstances = 100) # minority class를 numInstances 만큼 추가 생성 -> 이를 원데이터에 합쳐서 사용해야함
table(mwmote_result$Class) # minority class(positive) 100개 생성됨

# 기존 데이터와 MWMOTE를 활용해 오버샘플링한 결과를 합쳐 시각화
plotComparison(newthyroid1, rbind(newthyroid1, mwmote_result), attrs = names(newthyroid1)[1:3]) # 경계부근 데이터가 많이 추가된 것을 확인


# 2) RACOG(RApidly COnverging Gibbs)
#    Gibbs Sampler scheme을 활용해 분포 기반 새로운 데이터 생성
#    이산적(discrete) 타겟에만 적용
racog_result <- racog(newthyroid1, numInstances = 100)

# 기존 데이터와 RAcog를 활용해 오버샘플링한 결과를 합쳐 시각화
plotComparison(newthyroid1, rbind(newthyroid1, racog_result), attrs = names(newthyroid1)[1:3]) # 기존데이터 근처에 새로운 데이터 생성됨 확인


# 3) RWO(Random Walk Oversampling)
#    기존 데이터의 평균/분산이 유지되도록 데이터 생성
rwo_result <- rwo(newthyroid1, numInstances = 100)
# 기존 데이터와 RWO를 활용해 오버샘플링한 결과를 합쳐 시각화
plotComparison(newthyroid1, rbind(newthyroid1, rwo_result), attrs = names(newthyroid1)[1:3])


# 4) PDFOS(Probability Distribution density Function estimation based OverSampling)
#    minority class에 지역적으로 multivariate Gaussian kernel method를 적용해 새로운 데이터 생성
pdfos_result <- pdfos(newthyroid1, numInstances=100)
# 기존 데이터와 PDFOS를 활용해 오버샘플링한 결과를 합쳐 시각화
plotComparison(newthyroid1, rbind(newthyroid1, pdfos_result), attrs = names(newthyroid1)[1:3])


# 5) NEATER(filtering of oversampled data using non cooperaTive game theory)
#    오버샘플링된 데이터를 게임이론을 활용해 가치없는 데이터 필터링
# PDFOS를 활용해 오버샘플링한 데이터를 필터링
filtered <- neater(newthyroid1, newSamples = pdfos_result, iterations = 500)
table(filtered$Class) # 20개 필터링되어 80개의 positive 데이터
plotComparison(newthyroid1, rbind(newthyroid1, filtered), attrs = names(newthyroid1)[1:3])

# 6) Imbalance 패키지 내 oversampling을 쉽게 호출할 수 있는 method
#    oversample(data, ratio, method, filtering=TRUE, interation)
#    method : RACOG, wRACOG, PDFOS, RWO, ADASYN, ANSMOTE, BLSMOTE, DBSMOTE, BLSMOTE, DBSMOTE, SLMOTE, RSLSMOTE
filtered2 <- oversample(newthyroid1, ratio = 1, method = "PDFOS", filtering=TRUE, iterations = 500)
table(filtered2$Class) # negative 180 positive 132 (ratio = 1이라 minority class가 180개까지 oversampling 되었지만 필터링 과정에서 48개가 걸러져 최종 132개만 남음)

