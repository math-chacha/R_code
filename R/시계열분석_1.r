# 인하대학교 데이터사이언스학과 김승환 교수님 강의록


# 시계열 분석



## 1) 시계열 자료란 ?
##    시간의 흐름에 따라 관측된 값
##    시계열 자료의 구성 요인
##    - 불규칙 요인 : 시간에 따라 규칙적으로 움직이지 않는 Random한 요인으로 통계적 방법으로는 설명이 불가능한 요인
##    - 규칙 요인 :
##                 1. 추세 요인 : 시간이 경과함에 따라 지속적으로 증가하거나 감소하는 요인
##                 2. 계절 요인 : 계절에 의한 주기적 요인
##                 3. 순환 요인 : 계절에 의한 주기적 변동이 아니라 경기순환과 같이 큰 주기를 가지는 요인

## 2) 시계열 자료의 시각화
##    R code
##    ts(data, frequency = 1, start=c(start_year, start_month,...), ...)   frequency : 단위 시간 당 관측치 개수(12개월 -> 12) / start : 시작값

##    data : nybirths - 1946년 1월 ~ 1959년 12월 뉴욕의 월별 출생자수
births <- scan("http://robjhyndman.com/tsdldata/data/nybirths.dat")
head(births) # 26.663 23.598 26.931 24.740 25.806 24.364
birthsTS <- ts(births, frequency = 12, start=c(1946,1))
plot.ts(birthsTS, main = "New York births 1946.1 ~ 1959.12")


## 3) 시계열 분해
##    시계열을 불규칙 요인, 추세 요인, 계절 요인으로 분해
##    R code
##    decompose(ts.data)
birthsTSDecomp <- decompose(birthsTS)
plot(birthsTSDecomp)
# 분해 결과를 활용해 원 시계열에서 분리할 수도 있음
birthsTSSeasonAdj <- birthsTS - birthsTSDecomp$seasonal
par(mfrow=c(2,1))
plot(birthsTS, main = "원 데이터")
plot(birthsTSSeasonAdj, main = "계절성 제거")


## 4) 시계열 예측 방법
##    - 회귀분석방법(Regression method)
##      일반적으로 i.i.d(Independent and identically distributed) 데이터를 다룬다. 시계열 자료는 i.i.d가 아니므로 자기상관이 있다는 가정하에 전개되는 회귀분석이론 사용
##    - 평활법(Smoothing method)
##      예측원점에서 가까운 관측값에는 큰 가중치를 주고 먼 관측치일수록 작은 가중치를 주는 일종의 가중평균을 이용하는 방법
##      가중치를 주는 방법에 따라 이동평균법, 지수평활법으로 구분되며 이론적으로는 미흡하나 이해가 쉽고 계산이 용이한 장점이 있음
##    - 요소분해법(Decomposition method)
##      시계열 자료를 추세요인, 순환요인, 계절요인, 불규칙요인으로 분해하여 각각을 예측한 다음 다시 결합시키는 방법
##    - Box-Jenkin's 방법(ARIMA model)
##      이론적으로 우수한 방법이고 예측값이 가장 정확한 방법으로 널리 알려져 있음. 그러나 이해가 어렵고 계산이 어려워 전문가들이 주로 사용

##    - 회귀분석방법(Regression method)
##      R code
length(birthsTS) # 168

# 회귀분석
t <- c(1:168)
reg1 <- lm(births~t)
par(mfrow=c(1,1))
plot(t,births)
lines(t,births)
abline(reg1, col="red")

# 잔차분석
plot(t,reg1$residuals)
lag.plot(birthsTS,1) # 1개 시차 lag한 것과 원 데이터의 산점도 우상향하는 그림 보임
library(lmtest)
dwtest(reg1) # 오차항에 자기상관이 있는지 검정. DW =1.0733, p-value = 4.646e-10으로 귀무가설(자기상관이 없다) 기각 -> 자기상관 존재!!

# Cochrane-Orcutt Estimation
# 회귀분석으로 예측식을 만들어 잔차에 자기상관이 존재할 때, 자기상관을 업애는 방법으로 변수변환을 통해 회귀식을 추정하는 방법
# 해당 방법은 오차항이 1차 자기상관을 가질 때 많이 사용하는 방법
library(orcutt)
reg2 <- cochrane.orcutt(reg1)
reg2 # Y_t+1 = rho * Y_t + alpha * (1-rho) + beta * (X_t+1 - rho * X_t)
     # X_t = time, rho = 0.431913, alpha(intercept) = 21.719459, beta(t) = 0.038898
t <- c(1:168)

# 이전 시차 값 넣는 함수 생성
lag <- function(x)c(NA, x[1:length(x)-1])

# 1시차 전 값 생성
births1<-lag(births)

# 모형 파라미터 저장
rho <- 0.431913
alpha <- 21.719459
beta <- 0.038898

# 모형 대입
birthsEst <- rho * births1 + alpha * (1-rho) + beta * (t-rho*(t-1))

# 모형 적합 결과 시각화
plot(t, births, type="l",col="red", main = "predict with cochrane_orcutt regression")
lines(t, birthsEst, col="blue",lwd=3)

# 다음 값 예측 
births169 <- rho * births1[168] + alpha * (1-rho) + beta * (169-rho*168) # 27.74801

##    - 평활법(Smoothing method)
##      R code

# 단순지수평활법(Simple Expoential Smoothing)
# data : rain - 1813년 ~ 1912년 런던의 연간 강수량(inches)

# 데이터 불러오기
rain <- scan("http://robjhyndman.com/tsdldata/hurst/precip1.dat", skip=1)
rainTS <- ts(rain, start=c(1813))

# 시각화
plot.ts(rainTS, main="annual rainfall in LONDON 1813-1912")

# 모형 적합
rainTSForecasts <- HoltWinters(rainTS, beta = FALSE, gamma=FALSE)
rainTSForecasts # w = 0.02412151 으로 하면 예측오차 제곱합이 최소화 됨
plot(rainTSForecasts)

# 지수평활 활용한 예측
library(forecast)
rainTSForecasts2 <- forecast(rainTSForecasts, h=8) # 다음 8개 예측
rainTSForecasts2
plot(forecast(rainTSForecasts2))

# 잔차의 ACF(AutoCorrelation Function) 확인
acf(rainTSForecasts2$residuals, lag.max=20, na.action = na.pass) # lag1부터 ACF가 신뢰구간 안에 있는 것으로 보아 예측이 잘 이뤄졌음을 알 수 있음


# Double Exponential Smoothing
# Stochastic trend가 존재하는 시계열 적합 방법
# data : skirts - 1866년 ~ 1911년 여성의 치마 밑단 길이

# 데이터 불러오기
skirts <- scan("http://robjhyndman.com/tsdldata/roberts/skirts.dat",skip=5)
skirtsTS <- ts(skirts, start = c(1866))

# 시각화
plot.ts(skirtsTS, main = "annual skirt hem 1866-1911")

# 모형 적합
skirtSeriesForecasts <- HoltWinters(skirtsTS, gamma=FALSE)
skirtSeriesForecasts
plot(skirtSeriesForecasts)

# 예측
skirtSeriesForecasts2 <- forecast(skirtSeriesForecasts, h=19)
plot(forecast(skirtSeriesForecasts2))

# 잔차의 ACF(AutoCorrelation Function) 확인
acf(skirtSeriesForecasts2$residuals, lag.max=20, na.action = na.pass) # lag5에서 ACF가 신뢰구간을 벗어남 주의 필요


# Holt & Winter Exponential Smoothing
# stochastic trend와 계절성이 존재하는 시계열에 적합한 방법
# data : souvenir - 1987년 1월 ~ 1993년 12월 beach resort town의 souvenir(기념품) 가게의 월별 판매액의 log값
# 데이터 불러오기
souvenir <- scan("http://robjhyndman.com/tsdldata/data/fancy.dat")
souvenirTS <- ts(souvenir, frequency=12, start=c(1987,1))

# 시각화
plot.ts(souvenirTS) # 이분산성 감지

# 전처리
logSouvenirTS <- log(souvenirTS) # log변환 통한 정상시계열화
plot.ts(logSouvenirTS, main="time vs logSouvenir 1987.1-1993.12")

# 모형 적합
souvenirTSForest <- HoltWinters(logSouvenirTS)
souvenirTSForest
plot(souvenirTSForest)

# 예측
souvenirTSForest2 <- forecast(souvenirTSForest, h=48)
plot(forecast(souvenirTSForest2))

# 잔차의 ACF(AutoCorrelation Function) 확인
acf(souvenirTSForest2$residuals, lag.max=20, na.action = na.pass) # lag1부터 ACF가 신뢰구간 안에 있는 것으로 보아 예측이 잘 이뤄졌음을 알 수 있음


##    - Box-Jenkin's 방법(ARIMA model)
##      R code
##      example1. skirts
# data : skirts - 1866년 ~ 1911년 여성의 치마 밑단 길이

# 데이터 불러오기
skirts <- scan("http://robjhyndman.com/tsdldata/roberts/skirts.dat",skip=5)
skirtsTS <- ts(skirts, start = c(1866))

# auto.arima 활용한 ARIMA(p,d,q) 파라미터 찾기
library(forecast)
fit<-auto.arima(skirtsTS)

# 예측
forecast(fit, level=c(95), h=8)
plot(forecast(fit), shadecols="oldstyle") 
lines(fitted(fit),col="blue")

# 잔차의 ACF(AutoCorrelation Function) 확인
acf(fit$residuals, lag.max=8) # lag5에서 ACF가 신뢰구간을 벗어남 주의 필요

##      example2. souvenir
# data : souvenir - 1987년 1월 ~ 1993년 12월 beach resort town의 souvenir(기념품) 가게의 월별 판매액의 log값

# 데이터 불러오기
souvenir <- scan("http://robjhyndman.com/tsdldata/data/fancy.dat")
souvenirTS <- ts(souvenir, frequency=12, start=c(1987,1))

# 시각화
plot.ts(souvenirTS) # 이분산성 감지

# 전처리
logSouvenirTS <- log(souvenirTS) # log변환 통한 정상시계열화
plot.ts(logSouvenirTS, main="time vs logSouvenir 1987.1-1993.12")

# auto.arima 활용한 ARIMA(p,d,q) 파라미터 찾기
# auto.arima가 알아서 계절모형 고려해줌
library(forecast)
fit<-auto.arima(logSouvenirTS)
# ARIMAForecast<-Arima(logSouvenirTS, order=c(1,0,0), seasonal=c(1,1,0)) 직접 모형 생성도 가능

# 예측
forecast(fit, level=c(95), h=8)
plot(forecast(fit), shadecols="oldstyle") 
lines(fitted(fit),col="blue")


# 잔차의 ACF(AutoCorrelation Function) 확인
acf(fit$residuals, lag.max=8) # lag5에서 ACF가 신뢰구간을 벗어남 주의 필요



## 5) 기타 여러 시계열 모형
##    - ARIMAX
##      독립변수(외생변수)가 존재하는 경우의 ARIMA모형
##    - ARCH(AutoRegressive Conditional Hetroskedasticity) 코드 X
##      변동성이 특정 시점에 집중되는 경우 등분산성 가정이 있는 ARIMA 적합 어려움(로그 변환 하더라도 문제 해결 안되는 경우가 많음)
##      오차의 분산이 변하는 변동성 모형으로 위의 문제 해결
##    - GARCH(Generalized ARCH) 코드 X
##      ARCH를 개선한 모형
##    - NNAR(Neural Network Autoregressive) 
##      AR 구조를 신경망으로 학습하는 모형

##    - ARIMAX
##      R code
##      Arima(y, xreg=x, order...) 상수항을 넣으려면 include.drift=TRUE 옵션 추가
##      example. uschange
# data : fpp2::uschange - 1970년 ~ 2016년 분기별 미국 소비와 개인 소득
library(fpp2)

# 시각화
autoplot(uschange[,1:2], facets=TRUE) +
xlab("연도") + ylab("") +
ggtitle("미국 소비와 개인 소득의 분기별 변화")

# 모형 적합
fit <- auto.arima(uschange[,"Consumption"], xreg=uschange[,"Income"])
fit

# 잔차 확인 (잔차그림이 회귀모형 보다 i.i.d에 더 가까움)
cbind("Regression Errors" = residuals(fit, type="regression"),
"ARIMA errors" = residuals(fit, type="innovation")) %>%
autoplot(facets=TRUE)

# 잔차 분석(백색잡음 성질 만족하는지)
checkresiduals(fit) # p-value = 0.117 자기상관 계수들이 동시에 0이다

##    - NNAR(Neural Network Autoregressive) 
##      R code
# data : forecast::sunspotarea - 1875년 ~ 2015년 년도별 태양 흑점 면적
library(forecast)

# 모형 적합1
fit1 <- nnetar(sunspotarea) # 비선형 모형
autoplot(forecast(fit1,h=30))

# 모형 적합2
fit2 <- nnetar(sunspotarea, p=18, size=5) # 입력과 히든 노드 수 조절 가능
autoplot(forecast(fit2,h=30))


# 모형 정확도
accuracy(fit1)
accuracy(fit2)
