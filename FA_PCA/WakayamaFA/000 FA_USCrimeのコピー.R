#2 Factor Analysis - US Crime Data
#USCrime <- read.table("USCrime.txt", header=T)

# edited by jclee 2020年1月21日

library(dplyr)
USCrime<-read.table("UScrime.txt", header=T)

USCrime %>% colnames()

X <- USCrime[,-1]

rownames(X) <- abbreviate(USCrime[,1])

n <- nrow(X)
p <- ncol(X)
Z <- scale(X)

# R <- t(Z)%*%Z/(n-1)
# print("Correlation matrix"); 
# print(round(R,3))

svd.Z <- svd(Z/sqrt(n-1))
OBS <- svd.Z$u*sqrt(n-1)
VAR <- svd.Z$v%*%diag(svd.Z$d)
rownames(OBS) <- rownames(X)
rownames(VAR) <- colnames(X)
round(OBS,3)
round(VAR,3)
round(svd.Z$d^2,3)
biplot(OBS,VAR,cex=0.75,xlab="First Dimension",ylab="Second Dimension",xlim=c(-3,3),ylim=c(-3,3),main="Not Rotated")

# Varimax rotation
rotate <- function(VAR,theta)
    {
     G <- matrix(c(cos(theta),-sin(theta),sin(theta),cos(theta)),nrow=2)
     VAR.rotate <- VAR[,1:2]%*%G
     return(VAR.rotate)
    }
object.max <- function(theta)
    {
     D <- rotate(VAR[,1:2],theta)
     D2 <- D*D
     norm <- apply(D2,1,sum)
     temp <- scale(D2/norm,scale=F)
     return(sum(temp*temp))
    }
theta.optim <- optimize(object.max, c(-pi,pi), maximum=T)
theta <- theta.optim$maximum
VAR.rotate <- rotate(VAR[,1:2],theta)
OBS.rotate <- rotate(OBS[,1:2],theta)
c("optimum rotation in degrees"); round(theta*180/pi,1)
c("rotated loading matrix"); round(VAR.rotate,3)
c("rotated factor scores"); round(OBS.rotate,3)
x11()
biplot(OBS.rotate,VAR.rotate,cex=0.75,xlab="First Dimension",ylab="Second Dimension",
    xlim=c(-3,3),ylim=c(-3,3),main="Varimax Rotated")

# Factor analysis using library psych's function principal
library(psych)
prin.psych.none <- principal(R,nfactors=2,rotate="none")
summary(prin.psych.none)
round(prin.psych.none$loadings[,1:2],3)

prin.psych.rotate <- principal(R,nfactors=2,rotate="varimax")
summary(prin.psych.rotate)
round(prin.psych.rotate$loadings[,1:2],3)

# Factor analysis using R function factanal()
fact.X <- factanal(X,factors=2,rotation="none",scores="regression")
rownames(fact.X$scores) <- rownames(X)
rownames(fact.X$loadings) <- colnames(X)
round(fact.X$loadings,3)
round(fact.X$scores,3)
biplot(fact.X$scores,fact.X$loadings,cex=0.75,main="Unrotated ML Factor Solution")

fact.R <- factanal(X,factors=2,rotation="varimax",scores="regression")
rownames(fact.R$scores) <- rownames(X)
rownames(fact.R$loadings) <- colnames(X)
round(fact.R$loadings,3)
round(fact.R$scores,3)
biplot(fact.R$scores,fact.R$loadings,cex=0.75,main="Varimax Rotated ML Solution")

# end
