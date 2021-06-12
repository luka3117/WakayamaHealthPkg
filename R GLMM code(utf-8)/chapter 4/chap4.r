############################################
# R 활용 통계 모델링 입문 (박영사)
# 4장 R 실습 code
# 본서에 대한 문의사항은 박영사(www.pybook.co.kr) 또는 
# 역자 (이종찬, 이메일 : jc1010@korea.ac.kr)에게 연락바랍니다.
############################################

d <- read.csv("data3a.csv")

fit2 <- glm(y ~ x , family = poisson, data = d)
fit2

sum(log(dpois(d$y, lambda=d$y)))

fit.null<- glm(formula = y ~ 1, family =poisson, data=d)
fit.null
  
logLik(fit.null)
