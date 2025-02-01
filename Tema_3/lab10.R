
# Laboratorio 10 -----------------------------------------------------------

library(ggplot2)
library(forecast)
library(astsa)
library(car)
library(dlm)
library(numDeriv)


# 1. DLM de primer orden ------------------------------------------------------


?Nile
plot(Nile)

#  library(stats)
fitNile <- StructTS(Nile, "level")
fitNile

plot(Nile)
lines(fitted(fitNile), lty = 2, lwd = 2, col=2)
lines(tsSmooth(fitNile), lty = 4, lwd = 2 , col=3)
legend("topright",c("ajustado","suav."),lty =c(2,4),col=c(2,3))

tsdiag(fitNile)

plot(forecast(fitNile, level = c(50, 90), h = 10), xlim = c(1950, 1980))


#  library(dlm)

dlm.orden1 <- function(theta) {
  dlmModPoly(order = 1, dV = theta[1], dW = theta[2])
}
fit <- dlmMLE(Nile, parm = c(100, 2), dlm.orden1, lower = rep(1e-4, 2))
fit

modNile <- dlm.orden1(fit$par)
class(modNile)
str(modNile)

hs <- hessian(function(x) dlmLL(Nile, dlm.orden1(x)), fit$par)
all(eigen(hs, only.values = TRUE)$values > 0)

aVar <- solve(hs)
sqrt(diag(aVar))

smoothNile <- dlmSmooth(Nile, modNile)
names(smoothNile) 
#note que no nos da la variancia de del suavizamiento de Kalman.
#Para obtener la variancia, es necesario calcular U.S %*% D^2 %*% t(U.S)
var.Smooth1<-unlist(smoothNile$U.S) * smoothNile$D.S^2 * unlist(smoothNile$U.S)
var.Smooth2<-unlist(dlmSvd2var(smoothNile$U.S, smoothNile$D.S))
all(var.Smooth1==var.Smooth2)

hwidth <- qnorm(0.05, lower = FALSE) * sqrt(var.Smooth2)
sm <- cbind(smoothNile$s, as.vector(smoothNile$s) + hwidth %o% c(-1, 1))

ts.plot(sm,col=2,ylim=c(500,1500))
lines(Nile, type = 'o')

filterNile <- dlmFilter(Nile, modNile)
plot(residuals(filterNile, sd = FALSE), type = "o",
         ylab = "Standardized prediction error")
abline(h = 0)

tsdiag(filterNile)


foreNile <- dlmForecast(filterNile, nAhead = 10)
attach(foreNile)
hwidth <- qnorm(0.25, lower = FALSE) * sqrt(unlist(Q))
fore <- cbind(f, as.vector(f) + hwidth %o% c(-1, 1))
rg <- range(c(fore, window(Nile, start = c(1900, 1))))+c(0,300)
plot(fore, type = "o", pch = 16, plot.type = "s", lty = c(1, 3, 3),
         ylab = "Nile level", xlab = "", xlim = c(1900, 1980), ylim = rg)
lines(window(Nile, start = c(1900, 1)), type = 'o')


lines(window(smoothNile$s, start = c(1900,1)), lty = 5)
abline(v = mean(c(time(f)[1], tail(time(Nile), 1))),
           lty = "dashed", col = "darkgrey")
legend("topleft", lty = c(1, 5, 1, 3), pch = c(1, NA, 16, 16), bty = "n",
           legend = c("observed level", "smoothed level", "forecasted level",
                        "50% probability limits"))
detach(foreNile)


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

filteredNile <- dlmFilter(Nile, mod = modNile2)
resids <- residuals(filteredNile, sd = FALSE)

smoothNile <- dlmSmooth(Nile, modNile2)

theta1 <- dropFirst(smoothNile$s[, 1])
theta2 <- dropFirst(smoothNile$s[, 2])

par(mfrow = c(3, 1), mar = c(2.2, 2.2, 1, 1), cex = 0.8)
plot.ts(Nile, col = "darkgrey", xlab = "", ylab = "", lwd = 2)
lines(theta1, col = "black")
legend("topright", legend = c("Observación", "Estado"), 
       lwd = c(2, 1), col = c("darkgrey", "black"), bty = "n")

plot.ts(theta2, col = "darkgrey", xlab = "", ylab = "", 
        lwd = 2)
legend("topright", legend = "Pendiente", lwd = 2, col = "darkgrey", 
       bty = "n")
plot.ts(resids, ylab = "", xlab = "", col = "darkgrey", 
        lwd = 2)
abline(h = 0)
legend("topright", legend = "Residuales", lwd = 2, col = "darkgrey", 
       bty = "n")


tsdiag(filteredNile)

loglik2 <- dlmLL(Nile, modNile2)
n.coef <- 2
r.aic2 <- (2 * (loglik2)) + 2 * (sum(n.coef))  #dlmLL calcula el LL negativo
r.bic2 <- (2 * (loglik2)) + log(length(Nile)) * n.coef
r.aic2;r.bic2

