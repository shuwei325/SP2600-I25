
# Laboratorio 11 -----------------------------------------------------------

library(ggplot2)
library(forecast)
library(astsa)
library(car)
library(dlm)
library(numDeriv)


# 1. DLM de primer orden ------------------------------------------------------

?Nile
plot(Nile)

dlm.orden1 <- function(theta) {
  dlmModPoly(order = 1, dV = theta[1], dW = theta[2])
}
fit <- dlmMLE(Nile, parm = c(100, 2), dlm.orden1, lower = rep(1e-4, 2))
fit

modNile <- dlm.orden1(fit$par)
loglik1 <- dlmLL(Nile, modNile)
n.coef <- 2
r.aic1 <- (2 * (loglik1)) + 2 * (sum(n.coef))  #dlmLL calcula el LL negativo
r.bic1 <- (2 * (loglik1)) + log(length(Nile)) * n.coef
r.aic1;r.bic1

# 2. DLM de segundo orden ------------------------------------------------------

dlm.orden2 <- function(theta) {
  dlmModPoly(order = 2, dV = theta[1], dW = c(0,theta[2]))
}
fit2 <- dlmMLE(Nile, parm = c(1,1), dlm.orden2, lower = rep(1e-4,1e-4))
fit2

modNile2 <- dlm.orden2(fit2$par)
loglik2 <- dlmLL(Nile, modNile2)
n.coef <- 2
r.aic2 <- (2 * (loglik2)) + 2 * (sum(n.coef))  #dlmLL calcula el LL negativo
r.bic2 <- (2 * (loglik2)) + log(length(Nile)) * n.coef
r.aic2;r.bic2


# 3. AR (1) ------------------------------------------------------

# Siimulación AR(1) process
set.seed(12345)
y = arima.sim(n=250,list(ar=0.75,ma=0),sd=0.5)
ts.plot(y)

mod.ar = Arima(y,order=c(1,0,0),include.mean=FALSE)
mod.ar


# Configuración de un DLM
dlm0 = function(parm){
  return(dlm(FF=1,V=0,GG=parm[1],W=parm[2]^2,
             m0=0,C0=solve(1-parm[1]^2)*parm[2]^2))
}

# Estimación DLM
fitAR1 = dlmMLE(y=y,parm=c(0.7,1.0),build=dlm0,hessian=T)

# Estimaciones
(coef = fitAR1$par)

# Error estándar
var = solve(fitAR1$hessian)
sqrt(diag(var))


# Configuración del DLM estimado.
dlm0(fitAR1$par)

# Forecast next 5 observations using dlmForecast

mod = dlm0(fitAR1$par)
modf = dlmFilter(y,mod)
plot(modf$y)
points(modf$f,type="l",col=2)


#pronóstico
pronostico = dlmForecast(modf,nAhead=5,method="plain")
hwidth <- qnorm(0.1, lower = FALSE) * sqrt(unlist(pronostico$Q))
fore <- cbind(pronostico$f, as.vector(pronostico$f) + hwidth %o% c(-1, 1))
colnames(fore) <- c("forecast","li","ls")
fore

#Comparación con Arima
mod.ar = Arima(y,order=c(1,0,0),
               include.mean=FALSE)
forecast(mod.ar,h=5)

# 3. dlmModPoly y dlmModARMA ------------------------------------------------

n.obs = 500
yt = 2 + arima.sim(n=n.obs,list(ar=.75,ma=0),sd=.5)
plot(yt)

# La estimación sin restricción no converge
dlm1 = function(parm){
  dlm = dlmModPoly(1,dV=1e-7,dW=c(0)) +
    dlmModARMA(ar=parm[1], ma=NULL, sigma2=parm[2])
  # set initial state distribution
  dlm$C0[2,2] <- solve(1-parm[1]^2)*parm[2]
  return(dlm)
} 

# Estimación
fit1 = dlmMLE(y=yt,parm=c(0,0),build=dlm1,hessian=T)


# Solución: restricción de parámetros (estimar el log de la variancia)
parm_rest = function(parm){
  return( c(parm[1],exp(parm[2])) )
}

# Configuración de DLM
dlm1 = function(parm){
  parm = parm_rest(parm)
  dlm = dlmModPoly(1,dV=1e-7,dW=c(0)) +
    dlmModARMA(ar=parm[1], ma=NULL, sigma2=parm[2])
  # set initial state distribution
  dlm$C0[2,2] <- solve(1-parm[1]^2)*parm[2]
  return(dlm)
} 

# Estimación
fit1 = dlmMLE(y=yt,parm=c(0,0),build=dlm1,hessian=T)

# estimación de la parte AR(1)
(coef = parm_rest(fit1$par))

# Error estándar
var = solve(fit1$hessian)
sqrt(diag(var))

# Obtención de la estimación del intercepto 
mod1 = dlm1(fit1$par)
mod1filt = dlmFilter(yt,mod1)

# get parameters
coef = mod1filt$m[n.obs+1]
covar = dlmSvd2var(mod1filt$U.C[[n.obs+1]],mod1filt$D.C[n.obs+1,])
coef.se = sqrt(covar[1,1])
coef; coef.se

# dlmModSeas --------------------------------------------------------------

?UKgas
ts.plot(UKgas)
y = log(UKgas)
ts.plot(y)

# Especificación de DLM
dlm3 = dlmModPoly(2) + dlmModSeas(4)
dlm3_spec = function(x) {
  diag(W(dlm3))[2:3] = exp(x[1:2])
  V(dlm3) = exp(x[3])
  return(dlm3)
}
fit3 = dlmMLE(y,parm=c(0.1,0.1,0.1),build=dlm3_spec)
dlm3 = dlm3_spec(fit3$par)


loglik3 <- dlmLL(y, dlm3)
n.coef <- 3
r.aic3 <- (2 * (loglik3)) + 2 * (sum(n.coef))  #dlmLL caculates the neg. LL
r.bic3 <- (2 * (loglik3)) + (log(length(y))) * (n.coef)
r.aic3;r.bic3

ySmooth = dlmSmooth(y, mod = dlm3)

filteredgas <- dlmFilter(y, mod = dlm3)
resids <- residuals(filteredgas, sd = FALSE)
plot(resids)

x = cbind(y, dropFirst(ySmooth$s[, c(1, 3)]),dropFirst(resids))
colnames(x) = c("Gas", "Trend", "Seasonal","residual")
plot(x, type = "o", main = "UK Gas Consumption")

descomposicion=decompose(y)
plot(descomposicion)






