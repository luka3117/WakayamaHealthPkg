############################################
# R 활용 통계 모델링 입문 (박영사)
# 6장 R 실습 code
# 본서에 대한 문의사항은 박영사(www.pybook.co.kr) 또는 
# 역자 (이종찬, 이메일 : jc1010@korea.ac.kr)에게 연락바랍니다.
############################################

d <- read.csv("data4a.csv")

summary(d)


logistic <- function(z) 1 / (1 + exp(-z))
z <- seq(-6, 6, 0.1)
plot(z, logistic(z), type = "l", lwd = 2,
ylim = c(0, 1), yaxs = "i", xlab = "", ylab = "")


glm(cbind(y, N - y) ~ x + f, data = d, family = binomial)


library(MASS)   # stepAIC MASS package install
fit.xf<-glm(cbind(y, N - y) ~ x + f, data = d, family = binomial)
stepAIC(fit.xf)



glm(cbind(y, N - y) ~ x*f, family = binomial, data = d)


d <- read.csv("data4b.csv")

fit <- glm(y ~ x, offset = log(A), data = d, family = poisson)

y <- seq(-5, 5, 0.1)
plot(y, dnorm(y, mean = 0, sd = 1), type = "l")

pnorm(1.8, 0, 1) - pnorm(1.2, 0, 1)

dnorm(1.5, 0, 1)*0.6



load("d.RData")
fit <- glm(y ~ log(x), family = Gamma(link = "log"), data = d)
summary(fit)

