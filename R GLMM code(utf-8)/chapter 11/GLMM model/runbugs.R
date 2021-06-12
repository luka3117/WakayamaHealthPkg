############################################
# R 활용 통계 모델링 입문 (박영사)
# 11장 R 실습 code
# 본서에 대한 문의사항은 박영사(www.pybook.co.kr) 또는 
# 역자 (이종찬, 이메일 : jc1010@korea.ac.kr)에게 연락바랍니다.
############################################

source("../R2WBwrapper_Kor/R2WBwrapper(ver Kor).r") # 상위폴더의 R2WBwrapper 함수 호출
clear.data.param()

load("Y.RData")
set.data("N.site", length(Y))
set.data("Y", Y)

set.param("beta", 0)
set.param("r", rnorm(N.site, 0, 0.1))
set.param("s", 1)

post.bugs <- call.bugs(
	n.iter = 10100, n.burnin = 100, n.thin = 10
)
#post.list <- to.list(post.bugs)
#post.mcmc <- to.mcmc(post.bugs)
