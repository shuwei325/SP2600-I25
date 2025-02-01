# Laboratorio 13 -----------------------------------------------------------

library(fGarch)
library(rugarch)
library(astsa)
library(forecast)
library(quantmod)



arma.tgarch11.snorm = garchFit(r~arma(1,1)+garch(1,1),delta=1,leverage=T,cond.dist="snorm",
                               data=r,trace=F,include.mean=F)
arma.tgarch11.std = garchFit(r~arma(1,1)+garch(1,1),delta=1,leverage=T,cond.dist="std",
                             data=r,trace=F,include.mean=F)
arma.tgarch11.sstd = garchFit(r~arma(1,1)+garch(1,1),delta=1,leverage=T,cond.dist="sstd",
                              data=r,trace=F,include.mean=F)
arma.tgarch11.ged = garchFit(r~arma(1,1)+garch(1,1),delta=1,leverage=T,cond.dist="ged",
                             data=r,trace=F,include.mean=F)
arma.tgarch11.sged = garchFit(r~arma(1,1)+garch(1,1),delta=1,leverage=T,cond.dist="sged",
                              data=r,trace=F,include.mean=F)

norm(x, mean = 0, sd = 1, log = FALSE)

# normal asimétrica

x <- seq(-3,3,0.01)
snorm <- dsnorm(x, mean = 0, sd = 1, xi = 1.5, log = FALSE)
plot(x,snorm,type="l")


# t-student 
dstd(x, mean = 0, sd = 1, nu = 5, log = FALSE)

# t-student asimétrica
dsstd(x, mean = 0, sd = 1, nu = 5, xi = 1.5, log = FALSE)

# distribución de error generalizado
dged(x, mean = 0, sd = 1, nu = 2, log = FALSE)

# distribución de error generalizado simétrico
dsged(x, mean = 0, sd = 1, nu = 2, xi = 1.5, log = FALSE)

