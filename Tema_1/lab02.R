# Laboratorio 2 -----------------------------------------------------------

library(ggplot2)
library(forecast)
library(fpp2)
library(astsa)
library(tidyverse)
library(TSA)

# 1. Introducción y ejemplos -------------------------------------------------

# pasajeros
data(AirPassengers)
autoplot(AirPassengers)
autoplot(log(AirPassengers))+ 
  geom_smooth(method='lm', formula= y~x)
mod<-tslm(log(AirPassengers)~trend)
autoplot(mod$residuals)
autoplot(acf(mod$residuals,lag.max=50,plot=FALSE))
turista.per = mvspec(mod$residuals) 

# flujo del rio
data(flow)
autoplot(flow,ylab='River Flow')
autoplot(acf(flow,lag.max=50,plot=FALSE))
rio.per = mvspec(flow) 

# producción de leche
data(milk)
autoplot(milk)+ 
  geom_smooth(method='lm', formula= y~x)
mod<-tslm(milk~trend)
autoplot(mod$residuals)

autoplot(acf(mod$residuals,lag.max=50,plot=FALSE))
milk.per = mvspec(mod$residuals) 


# 1. Ilustración del comportamiento cíclico y periodicidad -------------------

# una señal de coseno + ruido
cs = 2*cos(2*pi*1:500/50 + .6*pi); w = rnorm(500,0,1)
par(mfrow=c(3,1), mar=c(3,2,2,1), cex.main=1.5)
plot.ts(cs, main=expression(2*cos(2*pi*t/50+.6*pi)))
plot.ts(cs+w, main=expression(2*cos(2*pi*t/50+.6*pi) + N(0,1)))
plot.ts(cs+5*w, main=expression(2*cos(2*pi*t/50+.6*pi) + N(0,25)))


# Suma de señales cíclicas
x1 = 2*cos(2*pi*1:100*6/100)  + 3*sin(2*pi*1:100*6/100)
x2 = 4*cos(2*pi*1:100*10/100) + 5*sin(2*pi*1:100*10/100)
x3 = 6*cos(2*pi*1:100*40/100) + 7*sin(2*pi*1:100*40/100)
x = x1 + x2 + x3 

par(mfrow=c(2,2))
tsplot(x1, ylim=c(-10,10), main = expression(omega==6/100~~~A^2==13))
tsplot(x2, ylim=c(-10,10), main = expression(omega==10/100~~~A^2==41))
tsplot(x3, ylim=c(-10,10), main = expression(omega==40/100~~~A^2==85))
tsplot(x, ylim=c(-16,16), main="sum")

# FFT
P = abs(2*fft(x)/100)^2
Fr = 0:99/100                    
tsplot(Fr, P, type="o", xlab="frequency", ylab="periodogram")
abline(v=.5, lty=2)

2^2+3^2    #=13
4^2+5^2    #41
6^2+7^2    #85

#con el periodograma del código "mvspec"
period<-mvspec(x)
period$freq
period$spec
plot(period$freq,period$spec, type='h')
periodograma<-data.frame(freq=period$freq,spec=period$spec)
periodograma[c(6,10,40),]
period$details

# 2. Ejemplo: Magnitud de estrella -------------------

TT = length(star)
par(mfrow=c(2,1))
tsplot(star, ylab="star magnitude", xlab="day")
Per   = Mod(fft(star-mean(star)))^2/TT
Freq  = (1:TT -1)/TT
tsplot(Freq[1:50], Per[1:50], type='h', lwd=3, ylab="Periodogram", xlab="Frequency")
text(.05,  7000, "24 day cycle") 
text(.027, 9000, "29 day cycle")
#en realidad corresponde a un ruido de omega+-delta
