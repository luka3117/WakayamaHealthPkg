############################################
# R 활용 통계 모델링 입문 (박영사)
# 3장 R 실습 code
# 본서에 대한 문의사항은 박영사(www.pybook.co.kr) 또는 
# 역자 (이종찬, 이메일 : jc1010@korea.ac.kr)에게 연락바랍니다.
############################################

d<-read.csv("data3a.csv")
d

d$x
d$y

d$f


class(d) # d는 data.frame 클래스
class(d$y) # y열은 정수만의 integer 클래스
class(d$x) # x열은 실수도 포함하므로 numeric 클래스
class(d$f) # 그리고 f열은 factor 클래스

summary(d)


# 그림 3.2 
plot(d$x, d$y, pch=c(21, 19)[d$f], cex=2)
legend("topleft", legend=c("C", "T"), pch=c(21,19),cex=1.5)

# 그림 3.3 
plot(d$f, d$y, pch=c(21, 19)[d$f], cex=2)


fit<-glm(y~x, data=d, family=poisson)

fit   # 또는 print(fit)도 가능함

summary(fit)

logLik(fit)

# 그림 3.7
plot(d$x, d$y, pch = c(21, 19)[d$f], cex=2)
xx <- seq(min(d$x), max(d$x), length = 50)
lines(xx, exp(1.29 + 0.0757 * xx), lwd = 2)

plot(d$x, d$y, pch = c(21, 19)[d$f], cex=2)
xx <- seq(min(d$x), max(d$x), length = 50)
yy<-predict(fit, newdata=data.frame(x=xx),type="response")
lines(xx, yy, lwd=2)


fit.f=glm(y~f, data=d, family=poisson)

fit.f

logLik(fit.f)

fit.all<-glm(y~x+f, data=d, family=poisson)

fit.all

logLik(fit.all)

