# Laboratorio 5 -----------------------------------------------------------

library(ggplot2)
library(forecast)
library(fpp2)
library(astsa)
library(tidyverse)
library(TSA)
library(MTS)
library(xts)
library(vars)
library(MASS)


# 0. Ejemplos (descriptivo) ---------------------------------------------------

par(mfrow = c(2,1))
tsplot(soi, ylab="", main="Southern Oscillation Index")
tsplot(rec, ylab="", main="Recruitment") 


par(mfrow=c(2,1))
acf1(soi, 48, main="Southern Oscillation Index")
acf1(rec, 48, main="Recruitment")


lag2.plot (soi, rec, 10) #importante ver la dirección que está calculando
lag2.plot (soi, rec, 5)

par(mfrow=c(2,1))
ccf2(rec,soi, 48, main="función de correlación cruzada de Rec. contra SOI")
ccf2(soi,rec, 48, main="función de correlación cruzada de SOI contra Rec.")

(r1=ccf(rec,soi, 10, plot=FALSE))
(r2=ccf(soi,rec, 10, plot=FALSE))



# 1. Ruido blanco Gaussiano -----------------------------------------------

#Con correlación instantánea
aa<-mvrnorm(n = 500, mu=c(0,0), Sigma=matrix(c(1.5,0.8,0.8,1),nrow=2))
colnames(aa)<-c("a1","a2")
aa<-data.frame(aa)
tsplot(aa)
acf2(aa$a1)
acf2(aa$a2)
acf(aa)
stats::acf(aa)

ccm(aa, lags= 10) 

mq(aa)

#Sin correlación instantánea
sig=diag(3)
aa2=mvrnorm(500,rep(0,3),sig)
tsplot(aa2)
stats::acf(aa2)

ccm(aa2, lags= 10) 

mq(aa2)




