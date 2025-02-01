
# Laboratorio 07 -----------------------------------------------------------

library(ggplot2)
library(forecast)
library(fpp2)
library(astsa)
library(car)
library(TSA)
library(tseries)
library(sarima)
library(vars)#
library(dse) 


# 1. Ejemplo de VAR(1) simulado ----------------------------------------------


## Especificar los parámetros del polinomio autoregresivo: phi(B) 
Phipoly   <- array(c(1.0, -0.5,
                   0, 0, 
                   0, -0.3, 
                   1, 0.5) ,
                 c(2, 2, 2))
#I-Phi_1 B
Phipoly[1,,]
Phipoly[2,,]
## Especificar la estructura de las inovaciones.
B <- diag(2)
## Especificar el término constante
TRD <- c(5, 10)
## Generación de VAR(2) 
var1  <- ARMA(A = Phipoly, B = B, TREND = TRD)
## Simulación de 500 observaciones
varsim <- simulate(var1, sampleT = 500,
                   noise = list(w = matrix(rnorm(1000),
                                           nrow = 500, ncol = 2)), rng = list(seed = c(123456))) 
## Obtención de la serie generada
vardat <- matrix(varsim$output, nrow = 500, ncol = 2)
colnames(vardat) <- c("x1", "x2")

plot.ts(vardat, main = "", xlab = "")

## Note la diferencia de mu_X con el vector de constante. ¿A qué se debe?
phi_B<-diag(2)+Phipoly[2,,]
solve(phi_B)%*%c(5,10)

acf(vardat)

## Determinar el lag de acuerdo a los criterios
infocrit <- VARselect(vardat, lag.max = 3,
                      type = "const")
infocrit

## Estimar el modelo
varsimest <- VAR(vardat, p = 1, type = "const",
                 season = NULL, exogen = NULL)

summary(varsimest)

## Verificar que los eigenvalues tengan módulo menor a 1 (estacionariedad)
roots <- vars::roots(varsimest)
roots
all(roots<1)


#Diagnósticos

(var2c.serial <- serial.test(varsimest, lags.pt = 16, type = "PT.asymptotic"))
(var2c.arch <- arch.test(varsimest, lags.multi = 5, multivariate.only = TRUE))
(var2c.norm <- normality.test(varsimest, multivariate.only = TRUE))

plot(var2c.serial, names = "x1")
plot(var2c.serial, names = "x2")

# Causalidad de Granger
(var.causal.x1<- causality(varsimest,cause="x2"))
(var.causal.x2<- causality(varsimest,cause="x1"))


# Pronóstico
predictions <- predict(varsimest, n.ahead = 25,
                       ci = 0.95)
fanchart(predictions)

# El análisis de impulso-respuesta

irf <- irf(varsimest)
plot(irf)
airf <- irf(varsimest, cumulative=TRUE)
plot(airf)

irf.x1 <- irf(varsimest, impulse = "x1",
              response = "x2",
              cumulative = FALSE)
plot(irf.x1)
airf.x1 <- irf(varsimest, impulse = "x1",
               response = "x2",
               cumulative = TRUE)
plot(airf.x1)


irf.x2 <- irf(varsimest, impulse = "x2",
              response = "x1",
              cumulative = FALSE)
plot(irf.x2)
airf.x2 <- irf(varsimest, impulse = "x2",
               response = "x1", n.ahead = 10,
               cumulative = TRUE)
plot(airf.y2)




# 2. Ejemplo de VAR(2) simulado ----------------------------------------------
#Code 2.1 tomado del Bernhard 2008

## Especificar los parámetros del polinomio autoregresivo: A(L) 
Phipoly2   <- array(c(1.0, -0.5, 0.3, 
                   0, 0.2, 0.1, 
                   0, -0.2, 0.7, 
                   1, 0.5, -0.3) ,
                 c(3, 2, 2))
#I-Phi_1 B - Phi_2 B
Phipoly2[1,,]
Phipoly2[2,,]
Phipoly2[3,,]
## Especificar la estructura de las inovaciones.
B <- matrix(c(1,0.8,0.8,1),nrow=2)
## Especificar el término de tendencia
TRD1 <- 5 + 0.05 * 1:500
TRD2 <- 10 + 0.02 * 1:500
TRD <- cbind(TRD1, TRD2)
## Generación de VAR(2) 
var2  <- ARMA(A = Phipoly2, B = B, TREND = TRD)
## Simulación de 500 observaciones
varsim <- simulate(var2, sampleT = 500,
                   noise = list(w = matrix(rnorm(1000),
                                           nrow = 500, ncol = 2)), rng = list(seed = c(123456))) 
## Obtención de la serie generada
vardat <- matrix(varsim$output, nrow = 500, ncol = 2)
colnames(vardat) <- c("x1", "x2")
plot.ts(vardat, main = "", xlab = "")

acf(vardat)

## Determinar el lag de acuerdo a los criterios
infocrit <- VARselect(vardat, lag.max = 3,
                      type = "both")
infocrit

## Estimar el modelo
varsimest <- VAR(vardat, p = 2, type = "both",
                 season = NULL, exogen = NULL)

summary(varsimest)

## Verificar que los eigenvalues tengan módulo menor a 1 (estacionariedad)
roots <- vars::roots(varsimest)
roots



# Diagnósticos ------------------------------------------------------------

## Prueba de Portmanteau
var2c.serial <- serial.test(varsimest, lags.pt = 16,
                            type = "PT.asymptotic")
var2c.serial
## Prueba de heteroscedasticidad
var2c.arch <- arch.test(varsimest, lags.multi = 5,
                        multivariate.only = TRUE)
var2c.arch
## Prueba de normalidad
var2c.norm <- normality.test(varsimest,
                             multivariate.only = TRUE)
var2c.norm
## Plot de los objetos "varcheck"
plot(var2c.serial, names = "x1")
plot(var2c.serial, names = "x2")

# Granger -------------------------------------------------------

var.causal.x1<- causality(varsimest,cause="x2")
var.causal.x1

var.causal.x2<- causality(varsimest,cause="x1")
var.causal.x2

# Pronóstico --------------------------------------------------------------

## Forecasting objects of class varest
predictions <- predict(varsimest, n.ahead = 25,
                       ci = 0.95)

## Plot of predictions for y1
class(predictions)
args(vars:::plot.varprd)

plot(predictions, names = "x1")
fanchart(predictions,names="x1")

## Fanchart for y2
fanchart(predictions, names = "x2")
plot(predictions, names = "x2")

# Función de impulso-respuesta --------------------------------------------

## Impulse response analysis
irf.x1 <- irf(varsimest, impulse = "x1",
              response = "x2", n.ahead = 10)
plot(irf.x1)
irf.x2 <- irf(varsimest, impulse = "x2",
              response = "x1", n.ahead = 10)
plot(irf.x2)

# Ejemplo: Mortalidad, temperatura y contaminación ---------------------------------

x = data.frame(cmort, tempr, part)
plot.ts(x , main = "", xlab = "")

(infocrit <- VARselect(x, lag.max = 10,
                       type = "both"))

fitvar= VAR(x, p=2, type="both")
summary(fitvar)

# Diagnósticos 

## Prueba de Portmanteau
var.serial <- serial.test(fitvar, lags.pt = 16,
                          type = "PT.asymptotic")
var.serial
plot(var.serial, names = "cmort")
plot(var.serial, names = "tempr")
plot(var.serial, names = "part")

## Prueba de heteroscedasticidad
var2c.arch <- arch.test(fitvar, lags.multi = 5,
                        multivariate.only = TRUE)
var2c.arch
## Prueba de normalidad
var2c.norm <- normality.test(fitvar,
                             multivariate.only = TRUE)
var2c.norm

res <- residuals(fitvar)
acf(res)

# Granger 
var.causal1<- causality(fitvar,cause="cmort")
var.causal1

var.causal2<- causality(fitvar,cause=c("tempr","part"))
var.causal2

#Pronóstico
predictions <- predict(fitvar, n.ahead = 25,
                       ci = 0.95)
plot(predictions)
fanchart(predictions)

fanchart(predictions, names = "cmort")



# Ejemplo: Canada ----------------------------------------------------

?Canada

data(Canada)
plot(Canada)

(infocrit <- VARselect(Canada, lag.max = 10,
                       type = "both"))

fitvar <- VAR(Canada, p = 2, type = "both")
summary(fitvar)

# Diagnósticos 

## Prueba de Portmanteau
var.serial <- serial.test(fitvar, lags.pt = 16,
                          type = "PT.asymptotic")
var.serial
plot(var.serial, names = "e")
plot(var.serial, names = "prod")
plot(var.serial, names = "rw")
plot(var.serial, names = "U")

## Prueba de heteroscedasticidad
var2c.arch <- arch.test(fitvar, lags.multi = 5,
                        multivariate.only = TRUE)
var2c.arch
## Prueba de normalidad
var2c.norm <- normality.test(fitvar,
                             multivariate.only = TRUE)
var2c.norm

# Granger 
var.causal1<- causality(fitvar,cause="e")
var.causal1


#Pronóstico
predictions <- predict(fitvar, n.ahead = 25,
                       ci = 0.95)
plot(predictions)
fanchart(predictions)
fanchart(predictions, name= "e")
fanchart(predictions, name= "U")

## Impulse response analysis
irf.U <- irf(fitvar, impulse = "e",
              response = "U", n.ahead = 10)
plot(irf.U)

