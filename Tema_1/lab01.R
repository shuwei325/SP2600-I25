# Laboratorio 1 -----------------------------------------------------------

library(ggplot2)
library(astsa)
library(tidyverse)
library(forecast)

# 1. Ilustración de procesos estocásticos ---------------------------------

#ruido blanco
w = rnorm(500,0,1) 
plot.ts(w, main="")
acf(w)

#medias móviles
v = stats::filter(w, sides=2, filter=rep(1/3,3)) # moving average
plot.ts(v, ylim=c(-3,3), main="medias móviles")
acf(v[-c(1,500)])


# 2. Ejemplos de ARMA con R ---------------------------------------------

#AR(1)
m<-5 #la media del proceso
y1 <- arima.sim(n = 150, model = list(order = c(1,0,0),ar = c(0.8)),sd=3,rand.gen= rnorm) + m
ts.plot(y1)
acf2(y1)

mod1<- forecast::Arima(y1, order = c(1, 0, 0))
summary(mod1)

# 3. modelo lineal --------------------------------------------------------

#ARMA(2,1)
phi=c(0.4,0.2)
theta=c(-0.6)

#Representación en MA
ARMAtoMA(ar=phi, ma=theta,lag.max=5)
#Representación en AR
ARMAtoAR(ar = phi, ma = theta, lag.max=5)

#Observen que convergen a cero!
plot(ARMAtoMA(ar=phi, ma=theta,lag.max=20))
plot(ARMAtoAR(ar=phi, ma=theta,lag.max=20))
