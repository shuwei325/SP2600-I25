# Laboratorio 3 -----------------------------------------------------------

library(ggplot2)
library(forecast)
library(fpp2)
library(astsa)
library(tidyverse)
library(TSA)

# 1. Ruido blanco, MA(1), AR(2) y AR(1) --------------------------------------

#Ruido blanco
arma.spec(ar = 0, ma = 0 ,main="Ruido blanco", col=4)
w = rnorm(100,0,1) 
plot.ts(w, main="")
mvspec(w)  

#MA(1)
par(mfrow=c(2,2))  
arma.spec(ar = 0 , ma =.5, main="MA(1)", col=4)
ma.sim <- arima.sim(list(order = c(0,0,1), ma = 0.5), n = 100)
ts.plot(ma.sim)
mvspec(ma.sim)  


#AR(2) con comportamiento periódico
par(mfrow=c(2,2))  
arma.spec(ar=c(1,-.9), ma= 0 , main="AR(2)", col=4) #periodico
ar2.sim <- arima.sim(list(order = c(2,0,0), ar = c(1,-0.9)), n = 100)
ts.plot(ar2.sim)
mvspec(ar2.sim)  

#AR(1) 
par(mfrow=c(2,2))  
arma.spec(ar=c(0.5), ma= 0 , main="AR(1)", col=4) #periodico
ar1.sim <- arima.sim(list(order = c(1,0,0), ar = c(0.5)), n = 100)
ts.plot(ar1.sim)
mvspec(ar1.sim)  

# 2. SOI y Reclutamiento de peces -------------------------------------------

# Se tiene la serie ambiental de índice de oscilación del sur (SOI, *Southern Oscillation Index*), 
# y la serie de número de peces nuevos (Reclutamiento) de 453 meses de 1950 a 1987.
# SOI mide cambios en presión relacionada a la temperatura del superficie del mar en el oceano pacífico 
# central, el cual se calienta cada 3-7 años por el efecto El Niño.

par(mfrow = c(2,2))
tsplot(soi, ylab="", main="SOI")
acf(soi, lag.max = 60)

tsplot(rec, ylab="", main="Reclutamiento") 
acf(rec, lag.max = 60)

# 4.1. Periodograma -------------------------------------------------------

par(mfrow=c(2,1))      
soi.per = mvspec(soi)             
abline(v=1/4, lty="dotted")
rec.per = mvspec(rec) 
abline(v=1/4, lty="dotted")

head(soi.per$details) 
tail(soi.per$details)


#SOI
soi.per$details[c(10,40),] 

U = qchisq(.025,2)
L = qchisq(.975,2)
# para frecuencia= 1/4
c(2*soi.per$spec[10]/L,2*soi.per$spec[10]/U)
# para frecuencia= 1
c(2*soi.per$spec[40]/L,2*soi.per$spec[40]/U)

#Rec

rec.per$details[c(10,40),] 

U = qchisq(.025,2)
L = qchisq(.975,2)
# para frecuencia= 1/4
c(2*rec.per$spec[10]/L,2*rec.per$spec[10]/U)
# para frecuencia= 1
c(2*rec.per$spec[40]/L,2*rec.per$spec[40]/U)






