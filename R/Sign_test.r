# 개념 : https://m.blog.naver.com/PostView.naver?isHttpsRedirect=true&blogId=lucifer246&logNo=189274624
# 코드 : https://m.blog.naver.com/PostView.naver?isHttpsRedirect=true&blogId=buillee&logNo=90171269371


# 부호검정(sign test)


# 1) 정의
#    분포의 중앙값에 대하여 검정하는 기법
#    H0 : mu = mu0
#    표본자료에 대해 mu0보다 큰 것에는 +, 작은 것에는 -부호 부여하고 부호 개수가 비슷하면 mu0에 대해 대칭이므로 귀무가설 채택
#    부호검정 통계량은 +인 것들의 개수
#    부호검정통계량은 보통 이항분포


# 2) code example
library(BSDA)
x <- c(25,16,44,82,36,58,18)
# H0 : 평균은 35다.
# H1 : 평균은 35보다 크다.
SIGN.test(x, md = 35, alternative = "greater") # s = 4, p-value = 0.5 -> 귀무가설 채택
# 즉, 평균은 35이다고 할 수 있다