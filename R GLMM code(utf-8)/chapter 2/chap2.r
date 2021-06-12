############################################
# R 활용 통계 모델링 입문 (박영사)
# 2 장 R 실습 code
# 본서에 대한 문의사항은 박영사(www.pybook.co.kr) 또는 
# 역자 (이종찬, 이메일 : jc1010@korea.ac.kr)에게 연락바랍니다.
############################################

# load("data.Rdata")   # 데이터 import
# 또는 직접 입력 
data<-c(2,2,4,6,4,5,2,3,1,2,0,4,3,3,3,3,4,2,7,2,4,3,3,3,4,3,7,5,3,1,7,6,4,6,5,2,4,7,2,2,6,2,4,5,4,5,1,3,2,3)
data 

length(data) # data에는 몇 개의 데이터가 포함되어 있는가?

summary(data) # data 요약

table(data) # 빈도표

hist(data, breaks=seq(-0.5, 9.5, 1)) # histogram

var(data) # variance

sd(data) # standard deviance
sqrt(var(data)) # standard deviance

y<-0:9
prob<-dpois(y, lambda=3.56) # poisson prob
plot(y, prob, type="b", lty=2) # plot

# Fig.2.3 
as.data.frame(cbind(y, prob))
N <- length(data)

# Fig.2.4 dpoist.set
plot(y, prob, type="b", lty=2)

# Fig.2.5 dpoist.set
# 관찰값과 히스토그램 겹쳐 그리기 
hist(data, breaks = seq(-0.5, 9.5))
lines(y, prob * N, type = "b", lty = 2)

# Fig.2.6 dpoist.set
# 모수에 따른 세가지 포아송분포
y <- 0:20
plot(y, dpois(y, lambda = 3.5), type = "b", lty = 2, pch = 21, ylab = "prob")
lines(y, dpois(y, lambda = 7.7), type = "b", lty = 2, pch = 23)
lines(y, dpois(y, lambda = 15.1), type = "b", lty = 2, pch = 24)
legend("topright", legend = c(3.5, 7.7, 15.1), pch = c(21, 23, 24),
title = "lambda", cex = 0.7)

# Fig. 2.7
# 모수에 포아송분포의 형태와 로그우도값 
logL <- function(m) sum(dpois(data, m, log = TRUE))
plot.poisson <- function(lambda) {
  y <- 0:9
  prob <- dpois(y, lambda = lambda)
  
  hist(data, breaks = seq(-0.5, 9.5, 1), ylim = c(0, 15),
       main = "", xlab = "", ylab = "")
  points(y, prob * 50)
  lines(y,  prob * 50, lty = 2)
  
  title(sprintf("lambda= %.1f\n logL= %.1f", lambda, logL(lambda)))
}

layout(matrix(1:9, byrow = T, ncol = 3)) # 그래프 출력 화면 3행 3열 분할 
sapply(seq(2, 5.2, 0.4), plot.poisson) # 그래프 출력

# Fig. 2.8
layout(matrix(1:1, byrow = T))  # 그래프 출력 화면을 디폴트로 복원

# 로그 우도함수의 점선 그래프  type = "b" 지정
logL <- function(m) sum(dpois(data, m, log = TRUE))
lambda <- seq(2, 5, 0.1)
plot(lambda, sapply(lambda, logL), type = "b")

# 로그 우도함수의 실선 그래프  type = "l" 지정
logL<-function(m) sum(dpois(data, m, log=TRUE))
lambda<-seq(2,5,0.1)
plot(lambda, sapply(lambda, logL) , type="l")

# Fig. 2.9
# 랜덤 포아송샘플 추출 함수 정의
# 표본수 50, 평균 3.5
r.poisson <-function(x) mean(rpois(50, 3.5))

# 3000개의 랜덤 포아송샘플 추출과 표본평균 계산
poi.rnd <- sapply(1:3000, r.poisson)
hist(poi.rnd, breaks=20, main="Histogram of Poisson ditribution \n with mean 3.5") # 히스토그램

