
# Laboratorio 12 -----------------------------------------------------------

library(fGarch)
library(rugarch)
library(astsa)
library(forecast)
library(quantmod)

# Ejemplo: promedio diario industrial Dow Jone (20 de abril, 20 --------


getSymbols("^DJI",from = "2016/12/31",
           to = "2018/12/31",
           periodicity = "daily")
y <- DJI$DJI.Close

r <- diff(log(y))[-1]
colnames(r)<-"r"

ts.plot(y)
ts.plot(r)

acf2(y)
acf2(r)

mod0 = auto.arima(r)
summary(mod0)
mod1 = Arima(r, order=c(1,0,1),include.mean = FALSE)
summary(mod1)
acf2(mod1$res)
checkresiduals(mod1,lag=20)

acf2(mod1$res^2)

fit.arch  = garchFit(r~garch(1,0),data=r,include.mean=FALSE)
fit.arch2  = garchFit(r~garch(1,0),data=r,include.mean=FALSE,trace=F)
fit.arch@fit$matcoef

plot(fit.arch)

#ARMA(1,1)+ARCH(1)
arma.arch1 <- garchFit(r~arma(1,1)+garch(1,0), data=r)
suppressWarnings(arma.arch1 <- garchFit(r~arma(1,1)+garch(1,0),include.mean=FALSE, data=r,trace=F))
arma.arch1@fit$matcoef

#ARMA(1,1)+GARCH(1,1)
suppressWarnings(arma.garch11 <- garchFit(r~arma(1,1)+garch(1,1),include.mean=FALSE, data=r,trace=F))
arma.garch11@fit$matcoef


# Criterio de información

fit.arch2@fit$ics #ARCH(1)
arma.arch1@fit$ics #ARMA(1,1)+ARCH(1)
arma.garch11@fit$ics #ARMA(1,1)+GARCH(1,1)

plot(arma.garch11)


# T-GARCH -----------------------------------------------------------------

tgarch11.1 = garchFit(r~garch(1,1),delta=1,leverage=T,data=r,trace=F,include.mean=F)
arma.tgarch11.1 = garchFit(r~arma(1,1)+garch(1,1),delta=1,leverage=T,data=r,trace=F,include.mean=F)

tgarch11.2 = garchFit(r~garch(1,1),delta=2,leverage=T,data=r,trace=F,include.mean=F)
arma.tgarch11.2 = garchFit(r~arma(1,1)+garch(1,1),delta=2,leverage=T,data=r,trace=F,include.mean=F)


tgarch11.1@fit$matcoef
arma.tgarch11.1@fit$matcoef

tgarch11.2@fit$matcoef
arma.tgarch11.2@fit$matcoef

tgarch11.1@fit$ics
arma.tgarch11.1@fit$ics
tgarch11.2@fit$ics
arma.tgarch11.2@fit$ics


# EGARCH ------------------------------------------------------------------

egarch11=ugarchfit(ugarchspec(mean.model=list(armaOrder=c(1,1)),
                        variance.model=list(model="eGARCH",garchOrder=c(1,1),
                        submodel=NULL,external.regressors=NULL, variance.targeting = FALSE)),r)
infocriteria(egarch11)
plot(egarch11)

# Todo parece indicar que TGARCH tiene mejor ajuste

plot(tgarch11.1,which=13)

arma.tgarch11.snorm = garchFit(r~arma(1,1)+garch(1,1),delta=1,leverage=T,cond.dist="snorm",
                             data=r,trace=F,include.mean=F)
arma.tgarch11.ged = garchFit(r~arma(1,1)+garch(1,1),delta=1,leverage=T,cond.dist="ged",
                             data=r,trace=F,include.mean=F)
arma.tgarch11.sged = garchFit(r~arma(1,1)+garch(1,1),delta=1,leverage=T,cond.dist="sged",
                             data=r,trace=F,include.mean=F)
arma.tgarch11.std = garchFit(r~arma(1,1)+garch(1,1),delta=1,leverage=T,cond.dist="std",
                             data=r,trace=F,include.mean=F)
arma.tgarch11.sstd = garchFit(r~arma(1,1)+garch(1,1),delta=1,leverage=T,cond.dist="sstd",
                             data=r,trace=F,include.mean=F)

arma.tgarch11.snorm@fit$ics
arma.tgarch11.ged@fit$ics
arma.tgarch11.sged@fit$ics
arma.tgarch11.std@fit$ics
arma.tgarch11.sstd@fit$ics

plot(arma.tgarch11.sged,which=13)

