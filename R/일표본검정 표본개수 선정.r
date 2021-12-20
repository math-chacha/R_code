# In-Kwon Yeo, "Sample Size Determination for One-Sample Location Tests", The Korean Journal of Applied Statistics (2015) 28(3), 573-581



# 일표본 검정의 표본 개수 결정

# 단측검정 베이스로 설명
## 1) t검정
##    평균이 mu이고 분산이 sigma^2인 정규모집단에서 확률분포 X1 ~ Xn을 얻었을 때 평균에 대한 검정통계량 Z = (X_bar / mu0) / (sigma / n^(1/2)) ~ N(0,1)
##    sigma를 알고 있다고 가정하고 pi를 표준정규분포라고 하자.
##    유의수준 alpha를 기준으로 기각역을 구할 수 있고 기각역의 임계값을 기준으로 H1 하에서의 검정력 1 - beta를 구할 수 있다.
##    이를 정리하면 Z_alpha - (mu1 - mu0) / (sigma / n^(1/2))  = -Z_beta를 유도할 수 있다.
##    이를 통해 표본크기 n은 sigma^2 * (Z_alpha + Z_beta)^2 / (mu1 - mu0)^2으로 계산할 수 있다.
##    r code
##    pwr::power.t.test(n = NULL, delta = NULL, sd = 1, sig.level = 0.05,
##             power = NULL,
##             type = c("two.sample", "one.sample", "paired"),
##             alternative = c("two.sided", "one.sided"),
##             strict = FALSE, tol = .Machine$double.eps^0.25)
library(pwr)
power.t.test(delta = 2, sd = 5, power= 0.8, type="paired") # 차이(delta)=2, 표준편차(sd)= 5, paired t test
# n = 51.00957 -> 52개 표본 필요


## 2) 부호검정
##    부호검정통계량 B = 표본 중 mu0보다 큰 값을 갖는 표본의 개수
##    일반적으로 부호검정을 사용하는 경우 표본크기 결정은 정규근사를 이용해 유도하는데 H0하에서 Var(B) = n/4, H1하에서 Var(B) = np(1-p)
##    이를 정리하면 (Z_alpha*(n/4)^(1/2) - (np-n/2) ) / (np(1-p))^(1/2) ~ -Z_beta
##    이를 통해 표본크기 n은 (Z_alpha / 2 + Z_beta * (p(1-p))^(1/2)) ^ 2 / (p-0.5)^2
##    분자의 p(1-p)를 1/4로 대체하여 간단하게 계산하기도 함
##    정규근사에 의한 표본크기결정 방법은 쉽게 적용할 수 있다는 장점이 있지만 원하는 수준의 유의수준과 검정력 보장 X
##    이를 보장받기 위해서는 정확검정에 의한 표본크기 계산 필요
