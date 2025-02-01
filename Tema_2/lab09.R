
# Laboratorio 09 -----------------------------------------------------------

library(ggplot2)
library(forecast)
library(fpp2)
library(astsa)
library(car)
library(TSA)
library(tseries)
library(sarima)
library(vars)
library(dse) 


# 1. Procesos cointegrados ------------------------------------------------

set.seed(123456)
a <- rnorm(1000)
b <- rnorm(1000)
x <- cumsum(a)
y <- 0.6*x + b
data <- ts(data.frame(x,y))
autoplot(data)



# Prueba de cointegración -------------------------------------------------

data(Raotbl3)
lc <- ts(Raotbl3$lc, start=c(1966,4), end=c(1991,2),
         frequency=4)
li <- ts(Raotbl3$li, start=c(1966,4), end=c(1991,2),
         frequency=4)
lw <- ts(Raotbl3$lw, start=c(1966,4), end=c(1991,2),
         frequency=4)
ukcons <- window(cbind(lc, li, lw), start=c(1967, 2),
                 end=c(1991,2))
autoplot(ukcons)

lc.eq <- summary(lm(lc ~ li + lw, data=ukcons))
li.eq <- summary(lm(li ~ lc + lw, data=ukcons))
lw.eq <- summary(lm(lw ~ li + lc, data=ukcons))
error.lc <- ts(resid(lc.eq), start=c(1967,2),
               end=c(1991,2), frequency=4)
error.li <- ts(resid(li.eq), start=c(1967,2),
               end=c(1991,2), frequency=4)
error.lw <- ts(resid(lw.eq), start=c(1967,2),
               end=c(1991,2), frequency=4)

par(mfrow=c(3,1))
plot(error.lc)
plot(error.li)
plot(error.lw)

checkresiduals(lc.eq)

ci.lc <- ur.df(error.lc, lags=1, type='none');summary(ci.lc)
ci.li <- ur.df(error.li, lags=1, type='none');summary(ci.li)
ci.lw <- ur.df(error.lw, lags=1, type='none');summary(ci.lw)
jb.lc <- jarque.bera.test(error.lc);jb.lc
jb.li <- jarque.bera.test(error.li);jb.li
jb.lw <- jarque.bera.test(error.lw);jb.lw

#ECM
x<-lc
y<-li
reg <- lm(y ~ x)
summary(reg)
error <- residuals(reg)
ADF <- ur.df(error, lags=1, type='none')
summary(ADF)
ts.plot(error)

error.lagged <- error[-c(1, 99)]
dy <- diff(y)
dx <- diff(x)
diff.dat <- data.frame(embed(cbind(dy, dx), 2))

#observen la diferencia
head(cbind(dy,dx))
head(diff.dat)

colnames(diff.dat) <- c('dy', 'dx', 'dyl1', 'dxl1')
ecm.dy <- lm(dy ~ error.lagged + dyl1 + dxl1,
              data=diff.dat)
summary(ecm.dy)

#ajustamos el modelo sin los coeficientes que no son significativos.
ecm.dy1 <- lm(dy ~ error.lagged,
               data=diff.dat)
summary(ecm.dy1)

ts.plot(ecm.dy1$residuals)
acf2(ecm.dy1$residuals)

ecm.dx <- lm(dx ~ error.lagged + dyl1 + dxl1,
             data=diff.dat)
summary(ecm.dx)


ts.plot(ecm.dy1$residuals)
acf2(ecm.dy1$residuals)


ts.plot(ecm.dx$residuals)
acf2(ecm.dx$residuals)

