############################################
# R 활용 통계 모델링 입문 (박영사)
# Fig  11.4 장 R 실습 code
# 본서에 대한 문의사항은 박영사(www.pybook.co.kr) 또는 
# 역자 (이종찬, 이메일 : jc1010@korea.ac.kr)에게 연락바랍니다.
############################################

library(R2WinBUGS)
if (!exists("Y")) load("Y.RData")
if (!exists("post.bugs")) load("post.bugs.RData")
#post.mcmc <- to.mcmc(post.bugs)
post.mcmc <- as.mcmc(post.bugs$sims.matrix)
v <- 1:length(Y)
y.max <- 27 

plot(
	v, Y,
	xlab = "location",
	ylab = "y_i",
	ylim = c(0, y.max)
)
lines(m, lwd = 2, lty = 2)

mre <- sapply(
	v, function(i) quantile(
		post.mcmc[, sprintf("r[%i]", i)],
		probs = c(0.5, 0.025, 0.975)
	)
)
b <- median(post.mcmc[,"beta"])
polygon(
	c(v, rev(v)),
	exp(b + c(mre[2,], rev(mre[3,]))),
	border = NA,
	col = "#00000030"
)
lines(exp(b + mre[1,]), lwd = 2)
