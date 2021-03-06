############################################
# R 활용 통계 모델링 입문 (박영사)
# 8장 R 실습 code
# 본서에 대한 문의사항은 박영사(www.pybook.co.kr) 또는 
# 역자 (이종찬, 이메일 : jc1010@korea.ac.kr)에게 연락바랍니다.
############################################

data <- c(4,3,4,5,5,2,3,1,4,0,1,5,5,6,5,4,4,5,3,4)

hist( data,  breaks = seq(-0.5, 8.5, 1))

n<-8
x <- 0:n
lines(x, length(data) * dbinom(x, n, prob = 0.45),  type = "b",  lty = 2)


