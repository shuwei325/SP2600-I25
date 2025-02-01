# Laboratorio 4 -----------------------------------------------------------

library(ggplot2)
library(forecast)
library(fpp2)
library(astsa)
library(tidyverse)
library(TSA)

# 1. Periodograma suavizado --------------------------------------------

#El núcleo utilizado
kernel('daniell',4)
plot(kernel("daniell", 4)) 

par(mfrow=c(2,1))
soi.ave = mvspec(soi, kernel('daniell',4))           
abline(v = c(.25,1,2,3), lty=2)
soi.rec = mvspec(rec, kernel('daniell',4))
abline(v=c(.25,1,2,3), lty=2)

length(soi) #n
soi.ave$n.used #n'
soi.ave$Lh # L
2*9*453/480
df  = soi.ave$df       # df = 16.9875  
U   = qchisq(.025, df) # U = 7.555916
L   = qchisq(.975, df) # L = 30.17425
soi.ave$spec[10]       # 0.0495202
soi.ave$spec[40]       # 0.1190800
# Intervalos de confianza
# para frecuencia= 1/4
round(c(df*soi.ave$spec[10]/L,df*soi.ave$spec[10]/U),4)
# para frecuencia= 1
round(c(df*soi.ave$spec[40]/L,df*soi.ave$spec[40]/U),4)

# 1.1. Periodograma suavizado en logarítmo ------------------------------------

soi.ave = mvspec(soi, kernel('daniell',4),log="yes")
abline(v = c(.25,1,2,3), lty=2)
rec.ave = mvspec(rec, kernel('daniell',4),log="yes")
abline(v=c(.25,1,2,3), lty=2)


# 2. Extensiones del periodograma suavizado -----------------------------

kernel("modified.daniell", c(10,10))          
plot(kernel("modified.daniell", c(10,10)))   

k        = kernel("modified.daniell", c(3,3))
soi.smo  = mvspec(soi, kernel=k, taper=.1)
abline(v = c(.25,1), lty=2)
## Repeat above lines with rec replacing soi 
soi.smo$df           # df = 17.42618
soi.smo$bandwidth    # B  = 0.2308103

# An easier way to obtain soi.smo:
soi.smo = mvspec(soi, spans=c(7,7), taper=.1, nxm=4)

# hightlight El Nino cycle
rect(1/7, -1e5, 1/3, 1e5, density=NA, col=gray(.5,.2))
mtext("1/4", side=1, line=0, at=.25, cex=.75)     



# 3. Algunos núcleos conocidos -----------------------------------------

plot(kernel("fejer", 10, r=6))   
plot(kernel("dirichlet", 10, r=6))   

# 4. Periodograma paramétrica ------------------------------------------

# Espectro de AR - orden=15 seleccionado de acuerdo al AIC
param.spec <- spec.ic(soi,  detrend=TRUE, col=4, lwd=2, nxm=4)  
# plot AIC and BIC
tsplot(0:30, param.spec[[1]][,2:3], type='o', col=2:3, xlab='ORDER', nxm=5, lwd=2, gg=TRUE)  

C.info<-data.frame(param.spec[[1]])
colnames(C.info)<-c("lag","AIC","BIC")
C.info %>% gather(
  key = "C.Info",
  value = "value",
  AIC,BIC
) %>% ggplot() +
  geom_line( aes(x = lag, y = value, group=C.Info,color=C.Info))


# 5. Algunas aplicaciones -------------------------------------------------
# Del libro de Cryer


# 5.1. Robot --------------------------------------------------------------

data(robot)
plot(robot,ylab='End Position Offset',xlab='Time')
acf(robot)

robot.per1 = mvspec(robot)  
robot.per2 = mvspec(robot, kernel('daniell',4))  
k        = kernel("modified.daniell", c(3,3))
robot.per3  = mvspec(robot, kernel=k, taper=0)
robot.per3  = mvspec(robot, kernel=k, taper=.1) #el concepto de tapering

param.specAIC <- spec.ic(robot,  col=4, lwd=2, nxm=4)  
param.specBIC <- spec.ic(robot, BIC= TRUE, col=4, lwd=2, nxm=4)  
param.specAIC[[1]]

# 5.1. Flujo de rio --------------------------------------------------------

data(flow)
plot(flow,ylab='River Flow')
acf(flow)

flow.per1 = mvspec(flow)  
flow.per1.log = mvspec(flow,log="yes")  
flow.per2 = mvspec(flow, kernel('daniell',4))  
k        = kernel("modified.daniell", c(3,3))
flow.per3  = mvspec(flow, kernel=k)
flow.per3.log = mvspec(flow, kernel=k,log="yes")  

param.specAIC <- spec.ic(flow,  col=4, lwd=2, nxm=4)  
param.specBIC <- spec.ic(flow, BIC= TRUE, col=4, lwd=2, nxm=4)  
param.specAIC[[1]]





