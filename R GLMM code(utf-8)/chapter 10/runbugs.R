############################################
# R 활용 통계 모델링 입문 (박영사)
# 10장 R 실습 code
# 본서에 대한 문의사항은 박영사(www.pybook.co.kr) 또는 
# 역자 (이종찬, 이메일 : jc1010@korea.ac.kr)에게 연락바랍니다.
############################################

source("./R2WBwrapper_Kor/R2WBwrapper(ver Kor).r")
d <- read.csv("data7a.csv")

clear.data.param()
set.data("N", nrow(d))
set.data("Y", d$y)

set.param("beta", 0)
set.param("r", rnorm(N, 0, 0.1))
set.param("s", 1)
set.param("q", NA)

post.bugs <- call.bugs(
	file = "model.bug.txt",
	n.iter = 10100, n.burnin = 100, n.thin = 10
)
#post.list <- to.list(post.bugs)
#post.mcmc <- to.mcmc(post.bugs)
