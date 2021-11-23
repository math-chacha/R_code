# 윤종식, 『ADP 실기 데이터 분석 전문가』, (주)데이터에듀-2020, 206-217

# 산점도


## 1. 산점도란
##    두 연속형 변수 간 관계를 보기 위해 좌표평면 상에 관측치를 찍어 나타낸 그림

## 2. plot함수 활용

##    R code
##    plot(x,y,xlab,ylab,main,pch,cex,col,xlim,ylim,type)
##    example(Cars93)
library(MASS)
attach(Cars93)
par(mfrow=c(1,1))

# Length와 Weight의 산점도(기본)
plot(Length, Weight) 

# x,y,제목 이름 생성(옵션 추가)
plot(Length, Weight, xlab="길이", ylab="무게", main="Length와 Weight의 산점도") 

# x,y 범위 생성(옵션추가)
plot(Length, Weight, xlim=c(130,230), ylim=c(1600,4400), xlab="길이", ylab="무게", main="Length와 Weight의 산점도") 

# 점종류 변경(옵션추가)
plot(Length, Weight, pch = "*", xlim=c(130,230), ylim=c(1600,4400), xlab="길이", ylab="무게", main="Length와 Weight의 산점도") 
plot(Length, Weight, pch = 8, xlim=c(130,230), ylim=c(1600,4400), xlab="길이", ylab="무게", main="Length와 Weight의 산점도")

# 점크기 변경(옵션추가)
plot(Length, Weight, cex = 2, pch = 8, xlim=c(130,230), ylim=c(1600,4400), xlab="길이", ylab="무게", main="Length와 Weight의 산점도")
plot(Length, Weight, cex = 0.5, pch = 8, xlim=c(130,230), ylim=c(1600,4400), xlab="길이", ylab="무게", main="Length와 Weight의 산점도")


# 점색상 변경(옵션추가)
plot(Length, Weight, col="red", cex = 0.5, pch = 8, xlim=c(130,230), ylim=c(1600,4400), xlab="길이", ylab="무게", main="Length와 Weight의 산점도")

# 그래프 종류 변경(옵션추가) p : 점 / l : 선 / b : 점과 선 + 만나는 곳에서는 점 또는 선 하나만  / o : 점과 선 중첩 / n : 그래프 초기화
plot(Length, Weight, type = "l", col="red", cex = 0.5, pch = 8, xlim=c(130,230), ylim=c(1600,4400), xlab="길이", ylab="무게", main="Length와 Weight의 산점도") 


# 선 종류 변경(옵션추가) 0 : blank / 1 : solid / 2 : dashed / 3 : dotted / 4 : dotdash / 5 : longdash / 6 : twodash
plot(Length, Weight, lty = 2, type = "l", col="red", cex = 0.5, pch = 8, xlim=c(130,230), ylim=c(1600,4400), xlab="길이", ylab="무게", main="Length와 Weight의 산점도") 

## 3. 그래프 서식

## R code
## par(mfrow=c(nr,nc)) : 한 화면에 nr행 nc열 그림 그리기
## legend(x,y,legend) : 범례(옵션 : pch, lty, fill, col, title, bg)
## example(1개 화면에 2개 plot그리기)
par(mfrow=c(1,2))
plot(Cars93$Length, Cars93$Weight, xlim=c(130,230), ylim=c(1600,4400), cex=2)
plot(Cars93$Length, Cars93$Weight, xlim=c(130,230), ylim=c(1600,4400), cex=0.5)

## example(범례)
par(mfrow=c(1,1))
plot(1:10, tpye="n", xlab=" ", ylab = " ", type="n") # 빈 좌표평면
# 문자열 위치 활용한 범례 (bottom, left, top, right, center)
legend("bottom", c("x1","y1"), pch=c(1,2), title="bottom", cex = 2) 
legend("center", c("x1","y1"), pch=c(3,4), title="bottom", cex = 2) 
# 좌표 위치 활용한 범례
legend(2.5,8, c("x1","y1"), pch=c(5,6), title="(2.5,8)", cex = 2) 
legend(7.5,4, c("x1","y1"), pch=c(7,8), title="(2.5,8)", cex = 2, bg="gray") 
