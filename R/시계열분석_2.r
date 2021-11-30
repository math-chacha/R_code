# https://ckmoong.tistory.com/10


# ADP 18회 시계열 분석 기출문제를 참고한 간단한 예제


## 1. 정상성 확인
## 2. ARIMA모델 3가지 제시 
## 3. 한 가지 모델 최종 선택 후 이유 서술
## 4. 최종 예측 후 실제 결과와 비교 평가하고 그 평가 방법을 사용한 이유 제시


## 1. 정상성 확인
##    정상성 : 평균 일정, 분산이 시점에 의존 X, 공분산은 시차에만 의존하고 시점에는 의존 X
##    정상 시계열 가공 방법 : 
##                         1) 평균이 일정하지 않다면 차분(Difference)
##                         2) 분산이 일정하지 않다면 변환(Transformation)
##    R code
# data : king - 42년 간 영국 왕들의 수명
library(forecast)

# 데이터 불러오기
king <- scan("http://robjhyndman.com/tsdldata/misc/kings.dat", skip=3)
head(king) # 60 43 67 50 56 42
kingTS <- ts(king)

# 데이터 시각화
plot.ts(kingTS,main = "영국 왕의 수명") # 시간에 따라 평균과 분산이 커지는 경향 발견

# 정상성 확인
# diff(x, lag=1, differences = 1, ...) lag : 사용할 lag / differences : 차분 차수
library(tseries)

new_kingTS <- diff(kingTS, d=1)
plot.ts(new_kingTS,main = "영국 왕의 수명 차분 1회")
adf.test(new_kingTS) # 차분한 시계열의 p-value 0.01654  -> 정상성 만족!



## 2. ARIMA모델 3가지 제시 
##    ACF / PACF 통한 Arima 최적 파라미터 찾기
##    ACF(Auto Correlation Function) : 시차에 따른 일련의 자기상관 
##    ACF가 임계값을 넘어서는 것의 lag(절단점)를 ARIMA에서 MA 계수로 사용
##    PACF(Partial ACF) : Y_t와 Y_t-k의 상관계수를 보는데 두 값 사이의 값들(각각에 영향을 준다고 생각할 수 있는 값)의 영향을 제거한 상관계수
##    PACF가 임계값을 넘어서는 lag(절단점)를 ARIMA에서 AR 계수로 사용

##    R code
acf(new_kingTS, lag.max=20) # lag 2
pacf(new_kingTS, lag.max=20) # lag 3
# 그러나 절단점이 명확하지는 않음
auto.arima(kingTS)
# ACF와 PACF 결과를 조합해 ARIMA 모델 제시
# 정상성을 만족하기 위해 차분을 한 번 실시했으므로 제시하는 ARIMA 모형 3개는 다음과 같음
# 1. ARIMA(0,1,1)
# 2. ARIMA(1,1,0)
# 3. ARIMA(1,1,1)

## 3. 한 가지 모델 최종 선택 후 이유 서술
arima011 <- Arima(kingTS, order = c(0,1,1))
arima110 <- Arima(kingTS, order = c(1,1,0))
arima111 <- Arima(kingTS, order = c(1,1,1))

summary(arima011) # AICc 344.44
summary(arima110) # AICc 352.36
summary(arima111) # AICc 346.39

# auto.arima 함수 실행결과 ARIMA(0,1,1)이 AICc가 가장 작게 나와 최적 모형으로 나옴


## 4. 최종 예측 후 실제 결과와 비교 평가하고 그 평가 방법을 사용한 이유 제시
##    데이터를 train 8 / test 2로 구분해 test값을 얼마나 잘 예측했는지 MSE를 활용해 평가
length(kingTS) # 42 -> train : 33 / test 9

train <- kingTS[1:(length(kingTS)-9)]
test <- kingTS[(length(kingTS)-8):length(kingTS)]

# 최종 선택한 모형에 train 데이터 넣어 재적합
fit <- Arima(train, order = c(0,0,1))


result <- Arima(test, model = fit)
accuracy(result) # RMSE 19.78126 RME 18.06982
# 예측값이 연속형자료이고 실제값과 스케일을 맞춰준 RMSE나 MAE로 평가

