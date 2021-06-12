############################################
# R 활용 통계 모델링 입문 (박영사)
# 11장 R 실습 code
# 본서에 대한 문의사항은 박영사(www.pybook.co.kr) 또는 
# 역자 (이종찬, 이메일 : jc1010@korea.ac.kr)에게 연락바랍니다.
############################################

source("set.data.R")

set.param("beta", rep(0, 4))
set.param("r", matrix(rnorm(N.site * 4, 0, 0.1), 4, N.site))
set.param("s", rep(1, 4))

post.bugs <- call.bugs(n.iter = 10100, n.burnin = 100, n.thin = 10)
post.list <- to.list(post.bugs)
post.mcmc <- to.mcmc(post.bugs)
