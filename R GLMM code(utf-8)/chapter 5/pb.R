get.dd <- function(d) # data generation and evaluate deviance
{
  n.sample <- nrow(d) # number of data
  y.mean <- mean(d$y) # sample mean
  d$y.rnd <- rpois(n.sample, lambda = y.mean)
  fit1 <- glm(y.rnd ~ 1, data = d, family = poisson)
  fit2 <- glm(y.rnd ~ x, data = d, family = poisson)
  fit1$deviance - fit2$deviance # return deviance
}

pb <- function(d, n.bootstrap)
{
  replicate(n.bootstrap, get.dd(d))
}
