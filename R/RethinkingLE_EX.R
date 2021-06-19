library(rethinking)

# 使用package
suppressMessages(library(tidyverse))



data(Howell1)
d<-Howell1

d %>% tbl_df()

d %>% dim()

d %>% colnames()

d<-d %>% filter(age>=18)

d %>% dim()

m4.3<-
  rethinking::map(alist(
  height~dnorm(mu, sigma),
  mu~beta0+beta1*weight,
  beta0~dnorm(156, 100),
  beta1~dnorm(0, 10),
  sigma ~ dunif(0, 50)
), data=d
)


m_add_age<-
  rethinking::map(alist(
  height~dnorm(mu, sigma),
  mu~beta0+beta1*weight+beta2*age,
  beta0~dnorm(156, 100),
  beta1~dnorm(0, 10),
  beta2~dnorm(0, 10),
  sigma ~ dunif(0, 50)
), data=d
)


precis(m4.3)
precis(m_add_age)
round( vcov( m4.3 ) , 3 )

plot(d$height~d$weight)

abline(coef(m4.3)["beta0"],
       coef(m4.3)["beta1"]
)

post<-extract.samples(m4.3)
post_age<-extract.samples(m_add_age)

post %>% dim()

post %>% class()

mu_at_50<-post$beta0 + post$beta1*50

mu_at_50 %>% length()


dens(mu_at_50)

link(m4.3) %>% dim()



HPDI(mu_at_50)
mu_at_50 %>% tbl_df()


post_age<-extract.samples(m_add_age)
post_age %>% class()
dens(post_age)
post %>% dim()


# ---------------------------------------------------
#  _     _____   ____
# | |   | ____| | __ )  __ _ _   _  ___  ___
# | |   |  _|   |  _ \ / _` | | | |/ _ \/ __|
# | |___| |___  | |_) | (_| | |_| |  __/\__ \
# |_____|_____| |____/ \__,_|\__, |\___||___/
#                            |___/
# ---------------------------------------------------

#
# - 女性平均寿命 Bayes Inference
# LE_FF_d_f=LE_d_f_final$LE_2015
#
# - 女性健康寿命 Bayes Inference
# HLE_FF_d_f=HLE_d_f_final$HLE_2016
#
# - 男性平均寿命 Bayes Inference
# LE_FF_d_m=LE_d_m_final$LE_2015
#
# - 男性健康寿命 Bayes Inference
# HLE_FF_d_m=HLE_d_m_final$HLE_2016
#
#


library(rethinking)
Bayes_fit_LE_d_f<-
  rethinking::map(alist(
  LE~dnorm(mu, sigma),
  mu~beta0+beta1*F1+beta2*F2,
  beta0~dnorm(80, 100),
  beta1~dnorm(0, 10),
  beta2~dnorm(0, 10),
  sigma ~ dunif(0, 50)
), data=LE_FF_d_f
)


precis(Bayes_fit_LE_d_f)
round( vcov(Bayes_fit_LE_d_f) , 3 )


post<-extract.samples(Bayes_fit_LE_d_f)


purrr::map(post, hist)


rethinking::HPDI(post$beta0)
rethinking::HPDI(post$beta1)
rethinking::HPDI(post$beta2)

rethinking::PI(post$beta0)
rethinking::PI(post$beta1)
rethinking::PI(post$beta2)

dens(post)

mu_at_Q1<-post$beta0 + post$beta1*mean(LE_FF_d_f$F1)+ post$beta2*quantile(LE_FF_d_f$F2)[1]
mu_at_Q2<-post$beta0 + post$beta1*mean(LE_FF_d_f$F1)+ post$beta2*quantile(LE_FF_d_f$F2)[2]
mu_at_Q3<-post$beta0 + post$beta1*mean(LE_FF_d_f$F1)+ post$beta2*quantile(LE_FF_d_f$F2)[3]
mu_at_Q4<-post$beta0 + post$beta1*mean(LE_FF_d_f$F1)+ post$beta2*quantile(LE_FF_d_f$F2)[4]
mu_at_Q5<-post$beta0 + post$beta1*mean(LE_FF_d_f$F1)+ post$beta2*quantile(LE_FF_d_f$F2)[5]


par(family= "HiraKakuProN-W3")

# par(mar=c(4,4,2,2)+0.1)
# par(mfrow=c(1,1))

# mu_at_Q1 %>% dens(main = "F2が Q1である場合")
# mu_at_Q1 %>% dens(main = "F2が Q2である場合", xlim=c(86.4, 87.4))
mu_at_Q2 %>% dens(main = "F2因子の変化による平均寿命分布の変化", xlim=c(86.4, 87.4))
mu_at_Q3 %>% dens(main = "F2が Q3である場合", add = T)
mu_at_Q4 %>% dens(main = "F2が Q4である場合", add = T)
mu_at_Q5 %>% dens(main = "F2が Q5である場合", add = T)


