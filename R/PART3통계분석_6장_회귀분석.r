# 윤종식, 『ADP 실기 데이터 분석 전문가』, (주)데이터에듀-2020, 180-202


# 회귀분석

## 1. 회귀분석이란
##    하나 이상의 독립변수가 종속변수에 미치는 영향을 추정하는 분석
##    가정
##    독립변수와 종속변수 간 선형성
##    오차의 등분산성
##    오차의 독립성
##    오차의 정규성
##    회귀 분석 후 진단(가정에 위배되지 않았는지) 필요


## 2. 단순선형회귀분석
##    하나의 독립변수가 종속변수에 미치는 영향 추정
##    Y_i = beta_0 + beta_1*x + epsilon
##    검토사항
##    1) 모형 내 회귀계수가 유의한가 -> 회귀 계수의 t통계량 p-value
##    2) 모형의 설명력 -> R square
##    3) 회귀모형은 유의한가 -> 회귀 모형의 F통계량 p-valeu
##    4) 모형의 적합도 -> 잔차 진단

##    R code
##    lm(formula, data)
##    plot(x, which) x : 선형회귀모형 / which : 그래프 종류 지정(1~6)
##    predict.lm(object, newdata, interval=c("none","confidence","prediction"), level)
##    example(Cars93의 EngineSize를 독립변수 가격(Price)를 종속변수로 회귀분석 실시)
library(MASS)
str(Cars93)
# 회귀분석
car_lm <- lm(Price ~ EngineSize, data=Cars93)
summary(car_lm) # 회귀 식 : Price = 5.5629 * EngineSize + 4.6692
                # 1) 회귀계수 유의한가 -> EngineSize의 t통계량 p-value : 2.59e-10 -> 유의
                # 2) 모형 설명력 -> adjusted R square = 0.3499 약 35% 설명
                # 3) 회귀모형 유의한가 -> 모형의 F통계량 p-value : 2.588e-10 -> 유의                
                # 4) 모형 적합도 -> plot(x, which) 함수 활용 
# 회귀진단
par(mfrow=c(2,3))
plot(car_lm,which=c(1:6)) # 1.residual vs Fitted : 오차 분포가 기울기가 0인 직선 형태 가지는 것이 좋음
                          # 2. Normal Q-Q : 표준화 잔차의 확률도로 정규성 만족한다면 점들이 45도 각도 직선 형태가 좋음
                          # 3. Scale-Location : x축=예측된 y값, y축=표준화 잔차로 기울기가 0인 직선 형태가 좋음 + 해당 직선에서 멀리 떨어진 것이 있다면 이는 이상치일 가능성
                          # 4. Cook's distance : cook's distance는 한 관측치가 회귀모형에 미치는 영향을 나타내는 측도로 일반적으로 1이상일 경우 매우 큰 영향 주는 관측값으로 판단
                          # 5. Residuals vs Leverage : x축=레버리지, y축=표준화잔차로 레버리지는 관측치가 다른 관측치 집단으로 부터 떨어진 정도. 빨간점선 밖에 있는 것은 예측치를 크게 벗어난 것
                          # 6. Cook's dist vs Leverage : x축=레버리지, y축=cook's distance 레버리지와 cook's distance는 비례하는 관계
# 예측
test_idx <- sample(1:nrow(Cars93),5)
test <- Cars93[test_idx,]
predict.lm(car_lm, test, interval="none")
predict.lm(car_lm, test, interval="confidence") 
predict.lm(car_lm, test, interval="prediction") # prediction일 때 회귀계수 불확실성과 오차항 감안해 confindence일 때보다 더 넓은 구간으로 예측


## 3. 다중선형회귀분석
##    두 개의 이상의 독립변수가 종속변수에 미치는 영향 추정 
##    Y_i = beta_0 + beta_1*x_1 + beta_2*x_2 + ... + beta_r*x_r + epsilon
##    검토사항
##    1) 모형 내 회귀계수가 유의한가 -> 회귀 계수의 t통계량 p-value
##    2) 모형의 설명력 -> R square
##    3) 회귀모형은 유의한가 -> 회귀 모형의 F통계량 p-valeu
##    4) 모형의 적합도 -> 잔차 진단
##    5) 다중공선성(독립변수 간 상관관계가 존재하는가) -> 상관계수/허용오차(1-R_i square)/VIF
##    범주형 변수 변환
##    dummy화 실시 필요
##    ex. 범주형 변수 A가 1,2,3 세 수준이라면 1,2에 여부에 대한 정보만 있어서 3 정보 추측가능하므로 dummy화를 실시하면 dummy_1, dummy_2 열만 생성됨

##    R code
##    범주형 변수의 dummy화
##    R의 lm함수는 범주형 변수를 자동으로 dummy화 해줌
##    example(Cars93 종속변수 Price에 EngineSize, RPM, Weight가 어떤 영향 주는지)
str(Cars93)
# 회귀분석
car_lm <- lm(Price ~ EngineSize + RPM + Weight, data=Cars93)
summary(car_lm) # 회귀 식 : Price = 4.305387 * EngineSize + 0.007096 * RPM + 0.007271 * Weight - 51.793292
                # 1) 회귀계수 유의한가 -> EngineSize의 t통계량 p-value : 0.00163 -> 유의
                #                     -> RPM의 t통계량 p-value : 1.22e-06 -> 유의
                #                     -> Weight의 t통계량 p-value : 0.00111 -> 유의
                # 2) 모형 설명력 -> adjusted R square = 0.5467 약 55% 설명
                # 3) 회귀모형 유의한가 -> 모형의 F통계량 p-value : 6.746e-16 -> 유의                
                # 4) 모형 적합도 -> plot(x, which) 함수 활용 
# 회귀진단
par(mfrow=c(2,3))
plot(car_lm,which=c(1:6)) # 설명 생략(2.단순선형회귀 부분 참고)

# 예측
# 단순선형회귀와 마찬가지로 진행


## 3. 최적회귀방정식 선택
##    1) 단계적 변수 선택
##       - 전진 선택법
##         절편만 있는 회귀모형에서 중요하다고 판단되는 독립변수를 차례로 모형에 추가하며 최적회귀방정식을 선택하는 방법
##       - 후진 제거법
##         모든 독립변수 포함한 모형에서 종속변수에 영향을 가장 적게 영향 주는 변수를 하나씩 제거하며 최적회귀방정식을 선택하는 방법
##       - 단계적 방법
##         전진선택법으로 변수를 추가하면서 추가한 후 기존 변수 중요도가 떨어지면 제거해 최적회귀방정식을 선택하는 방법
##    2) 벌점화된 선택기준
##       - AIC
##       - BIC
##         작을수록 좋은 모형

##    R code
##    step(object, scope, direction, k)  scope : 변수 선택 과정에서 사용되는 모형 범위 / direction : 변수 선택 방법("forward","backward","stepwise") / k : 모형 선택 기준(k=2 : AIC, k=log(데이터 수) : BIC)
##    example(Cars93 종속변수 Price에 EngineSize, Horsepower, RPM, Width, Length, Weight가 어떤 영향 주는지)
car_lm <- lm(Price ~ EngineSize+Horsepower+RPM+Width+Length+Weight, data=Cars93)
step(car_lm, direction="backward") # 회귀 식 : Price = 0.129653 * Horsepower - 1.480623 * Width + 0.152968 * Length + 0.007339 * Weight + 53.005861
                                   # 후진선택법 사용 결과 EngineSize, RPM 순으로 제거됨