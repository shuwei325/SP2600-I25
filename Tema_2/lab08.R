
# Laboratorio 08 -----------------------------------------------------------

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


# 1. Consumo UK ----------------------------------------------

data(Raotbl3)
lc <- ts(Raotbl3$lc, start=c(1966,4), end=c(1991,2), frequency=4)
plot(lc)

#contraste de Dickey-Fuller con library(tseries)
adf.test(lc,k=3)

# Estimar el modelo completo
lc.ct <- ur.df(lc, lags=3, type='trend')
plot(lc.ct) #comportamiento de los residuales de este modelo.
summary(lc.ct)

#realizar una diferencia
lc.dif <- diff(lc)
plot(lc.dif)

adf.test(lc.dif,k=3) #siempre incluye la tendencia

lc2.ct <- ur.df(lc.dif, type='none', lags=3)
summary(lc2.ct)
plot(lc2.ct)

# 2. Producto nacional bruto - U.S. (ejemplo 3.40, Shumway&Stoffer) ----------------------------------------------------------------
# producto nacional bruto, U.S. (en mil millones y son datos trimestrales de 1947 a 2002)
# los datos fueron ajustada estacionalmente.

y<-astsa::gnp
ts.plot(y)
acf2(y, 50)           

#contraste de Dickey-Fuller con library(tseries)
adf.test(y)

# Estimar el modelo completo
df.y <- ur.df(y, lags=6, type='trend')
plot(df.y) #comportamiento de los residuales de este modelo.
summary(df.y)


#realizar una diferencia
y.dif <- diff(y)
plot(y.dif)

adf.test(y.dif) #siempre incluye la tendencia

df.y.dif <- ur.df(y.dif, type='none', lags=3)
summary(df.y.dif)
plot(df.y.dif)


# 5.logarítmo de varve glacial   (ejemplo 3.41, Shumway&Stoffer)-----------------------------------------------
# Los glaciares que se derriten depositan capas anuales de arena y limo durante 
# las temporadas de derretimiento de primavera, que pueden reconstruirse anualmente 
# durante un período que va desde el momento en que comenzó la desglaciación en Nueva 
# Inglaterra (hace unos 12.600 años) hasta el momento en que terminó (hace unos 6000 años).
# Dichos depósitos sedimentarios, llamados varvas, pueden utilizarse como sustitutos de 
# parámetros paleoclimáticos, como la temperatura, porque, en un año cálido, se depositan 
# más arena y limo del glaciar en retroceso.

y <- astsa::varve
plot(y)

logy = log(y)

plot(logy)
acf2(logy,50)

#contraste de Dickey-Fuller con library(tseries)
adf.test(logy)

# Estimar el modelo completo
df.logy <- ur.df(logy, lags=6, type='none')
plot(df.logy) #comportamiento de los residuales de este modelo.
summary(df.logy)


#realizar una diferencia
y.dif <- diff(logy)
plot(y.dif)

adf.test(y.dif) #siempre incluye la tendencia

df.y.dif <- ur.df(y.dif, type='none', lags=3)
summary(df.y.dif)
plot(df.y.dif)

