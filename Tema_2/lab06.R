# Laboratorio 6 -----------------------------------------------------------

library(ggplot2)
library(forecast)
library(fpp2)
library(astsa)
library(tidyverse)
library(TSA)
library(MTS)
library(marima)
library(dse) 

# 1. VAR --------------------------------------------

#El producto interno bruto de Reinos Unidos, Canadá y Estados Unidos de segundo trimestre del 1980 al segundo trimestre del 2011.
#Los datos son ajustados estacionalmente.

da=read.table("q-gdp-ukcaus.txt",header=T)
tsplot(da[,3:5])
loggdp=log(da[,3:5])
tsplot(loggdp)
x=loggdp[2:126,]-loggdp[1:125,]
x=x*100
tsplot(x)

acf(x)

m1=MTS::VAR(x,1)
m2=MTS::VAR(x,2)

#comparación con el paquete vars
mvars=vars::VAR(x,p=1)
summary(mvars)

m3=VARorder(x,maxp=15)

CI=data.frame(order=0:15,AIC=m3$aic,BIC=m3$bic,HQ=m3$hq)
CI%>%gather(
  key = "C.Info",
  value = "value",
  AIC,BIC,HQ
) %>% ggplot() +
  geom_line( aes(x = order, y = value, group=C.Info,color=C.Info))

mq(m1$residuals,adj=9)
1*3^2#p*k^2
mq(m2$residuals,adj=18)
2*3^2#p*k^2
#
MTSdiag(m2,adj=18)

# 2. VMA ---------------------------------------------------------------------

# Retornos en logaritmo (en porcentaje) de un portafolio de CRSP (Center for Research in Security Prices),
# de enero 1961 a diciembre 2011 (T=612).
# Consiste en stocks de NYSE, AMEX y NASDAQ.
# Se va a estudiar el decil 5 y decil 8 del  logretorno.
 

da=read.table("m-dec15678-6111.txt",header=T)
head(da)
x=log(da[,2:6]+1)*100
rtn=cbind(x$dec5,x$dec8)
tdx=c(1:612)/12+1961
par(mfcol=c(2,1))
plot(tdx,rtn[,1],type='l',xlab='year',ylab='d5')
plot(tdx,rtn[,2],type='l',xlab='year',ylab='d8')

ccm(rtn)
MTS::VMAorder(rtn,lag=20)
m1=MTS::VMA(rtn,q=1)
m2=MTS::VMA(rtn,q=2)
m1$bic
m2$bic

#Probemos con VAR
m3=MTS::VARorder(rtn,maxp=15)
m4=MTS::VAR(rtn,p=4)

mq(m4$residuals,adj=16)
2^2*4#k^2*(p)
#
MTSdiag(m4,adj=16)
m4$bic
m1$bic
m2$bic

m5=MTS::VAR(rtn,p=2)

mq(m4$residuals,adj=8)
2^2*2#k^2*(p)
#
MTSdiag(m4,adj=8)
m4$bic
m1$bic
m2$bic


