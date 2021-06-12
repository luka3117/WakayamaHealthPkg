############################################
# R 활용 통계 모델링 입문 (박영사)
# 7장 R 실습 code
# 본서에 대한 문의사항은 박영사(www.pybook.co.kr) 또는 
# 역자 (이종찬, 이메일 : jc1010@korea.ac.kr)에게 연락바랍니다.
############################################

d <- read.csv("data.csv")
d4 <- d[d$x==4,]
table(d4$y)
c(mean(d4$y), var(d4$y))


library(glmmML)
glmmML(cbind(y, N-y) ~ x, data = d, family = binomial, cluster = id, method="ghq")



