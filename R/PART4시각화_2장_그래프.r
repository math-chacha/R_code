# 윤종식, 『ADP 실기 데이터 분석 전문가』, (주)데이터에듀-2020, 218-233

# 그래프

## 1. 점그래프

##    R code
##    points(x, y=NULL, pch, cex, col)
##    example(iris Petal.Length, Petal.Width)
plot(NULL, type="n", xlim=c(0,8), ylim=c(0,3), xlab="Petal.Length", ylab="Petal.Width", main="점그래프 예시")
points(iris$Petal.Length, iris$Petal.Width)

## 2. 선그래프

##    R code
##    lines(x, y=NULL, lty, lwd, col ...) 꺾은선 그래프
##    abline(a=NULL, b=NULL, h=NULL, v=NULL, coef=NULL, reg=NULL,...) 직선 그래프
##    curve(expr, from=NULL, to=NULL, ...) 곡선그래프 expr : 시각화할 함수 또는 표현식

##    example(꺾은선 그래프)
plot(NULL, type="n", xlim=c(0,8), ylim=c(0,3), xlab="", ylab="", main="선그래프 예시")
lines(c(0,8),c(3,3), lty=1)
lines(c(0,7),c(2.5,2.5), lty=2)
lines(c(0,6),c(2,2), lty=3)
lines(c(0,5),c(1.5,1.5), lty="longdash", lwd=4)
lines(c(0,4),c(1,1), lty="longdash", lwd=1)

##    example(직선 그래프)
plot(cars, ylim=c(0,130), xlim=c(0,30), main="cars")

# 회귀식 추가
abline(lm(dist~speed, data=cars), col="red")

# x축 중위수 수직선
abline(v=median(cars$speed), lty="dotdash")

# y축 중위수 수평선
abline(h=median(cars$dist), lty="dotdash")

##    example(곡선 그래프)
curve(dnorm(x,mean=0,sd=1), from = -3, to = 3, xlab="x", ylab="density", main = "정규분포")


## 3. 막대 그래프

##    R code
##    barplot(height, names.arg, space, horiz, main, xlab, ylab, col, beside, xlim, ylim)
##    height : 막대그래프 데이터 벡터 / names.arg : 막대그래프 이름 벡터 / space : 막대그래프 간격 / horiz : 세로 표현 여부 / beside : 범주가 여러 개일 경우 병렬로 연결된 막대 그래프 표현 여부
##    example(단일 범주형 변수 막대그래프)
par(mfrow=c(1,2))
barplot(table(Cars93$Origin), ylim=c(1,50), xlab = "Origin", ylab = "도수")
barplot(table(Cars93$Cylinders), ylim=c(1,50), xlab = "Cylinders", ylab = "도수")

##    example(여러 범주형 변수 막대그래프)
par(mfrow=c(1,2))
barplot(table(Cars93$Origin, Cars93$Cylinders), beside=F, ylim=c(0,60), legend=T)
barplot(table(Cars93$Origin, Cars93$Cylinders), beside=T, ylim=c(0,60), legend=T)


## 4. 히스토그램
##    히스토그램 vs 막대그래프
##    히스토그램 : 연속형 / 막대그래프 : 이산형

##    R code
##    hist(x, main, xlab, ylab, breaks, freq, col)
##    breaks : 막대 구간 지정 / freq : y축에 구간별 데이터 개수 표시 여부
##    example
par(mfrow=c(1,2))
hist(iris$Petal.Length, main="breaks default")
hist(iris$Petal.Length, breaks=5, main="breaks 5")


## 5. 파이차트

##    R code
##    pie(x,labels, col, lty, main)
##    example
par(mfrow=c(1,1))
pie(table(Cars93$Cylinders), main="Cylinders pie chart")


## 6. 산점도 행렬

##    R code
##    pairs(x, labels,...)
##    pairs(formula, data=NULL, subset,...)
##    example
pairs(~Sepal.Length + Sepal.Width+Petal.Length+Petal.Width,
data=iris, col=c("red","green","blue")[iris$Species])
