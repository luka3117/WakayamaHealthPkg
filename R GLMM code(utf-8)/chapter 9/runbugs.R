############################################
# R 활용 통계 모델링 입문 (박영사)
# 9장 R 실습 code
# 본서에 대한 문의사항은 박영사(www.pybook.co.kr) 또는 
# 역자 (이종찬, 이메일 : jc1010@korea.ac.kr)에게 연락바랍니다.
############################################

source("./R2WBwrapper_Kor/R2WBwrapper(ver Kor).r") # R2WBwrapper 호출
load("d.RData") # 데이터 로딩

# 초기치 설정
clear.data.param()
set.data("N", nrow(d))
set.data("Y", d$y)
set.data("X", d$x)
set.data("Mean.X", mean(d$x))

set.param("beta1", 0)
set.param("beta2", 0)

# 사후분포 샘플링
post.bugs <- call.bugs(
	file = "model.bug.txt",
	n.iter = 1600, n.burnin = 100, n.thin = 3
)
post.mcmc <- to.mcmc(post.bugs)
