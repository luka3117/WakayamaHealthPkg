############################################
# R 활용 통계 모델링 입문 (박영사)
# 5장 R 실습 code
# 본서에 대한 문의사항은 박영사(www.pybook.co.kr) 또는 
# 역자 (이종찬, 이메일 : jc1010@korea.ac.kr)에게 연락바랍니다.
############################################

d<-read.csv("data3a.csv")

fit2 <- glm(y ~ x, data = d, family = poisson)
fit1 <- glm(y ~ 1, data = d, family = poisson)

fit2$deviance

fit1$deviance-fit2$deviance

d$y.rnd<-rpois(100, lambda = mean(d$y))


fit1 <- glm(y.rnd ~ 1, data = d, family = poisson)
fit2 <- glm(y.rnd ~ x, data = d, family = poisson)
fit1$deviance-fit2$deviance

source("pb.R") # infile pb.R
dd12<-pb(d, n.bootstrap = 1000)

summary(dd12)

hist(dd12, 100, xlab="", ylab="" , main="")

abline(v= 4.5, lty = 2)

sum(dd12 >= 4.5)

quantile(dd12, 0.95)



fit1 <- glm(y ~ 1, data = d, family = poisson)
fit2 <- glm(y ~ x, data = d, family = poisson)
anova(fit1, fit2, test = "Chisq")
