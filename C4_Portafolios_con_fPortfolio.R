# =========================================================
#     Portafolio con 3 activos usando fPortfolio
# =========================================================

library(quantmod)
library(fPortfolio)


# 1. Precios de activos de Apple, Microsoft y Oracle

precios <- c("AAPL", "MSFT", "ORCL")
getSymbols(precios, from = "2019-01-02", periodicity = "daily")

R.ap <- diff(log(AAPL$AAPL.Close)) * 100
R.mi <- diff(log(MSFT$MSFT.Close)) * 100
R.or <- diff(log(ORCL$ORCL.Close)) * 100


# 2. Matrices

mu <- rbind(mean(R.ap[-1]), mean(R.mi[-1]), mean(R.or[-1])); mu
Datos <- data.frame(R.ap[-1], R.mi[-1], R.or[-1]); Datos
S <- cov(Datos); S

i <- matrix(c(1, 1, 1), 3)
Si <- solve(S) %*% i
Su <- solve(S) %*% mu

A <- c(t(i) %*% solve(S) %*% mu)
B <- c(t(i) %*% solve(S) %*% i)
C <- c(t(mu) %*% solve(S) %*% mu)
D <- A^2 - B^2


# 3. Portafolio con retorno 0.105 y mínimo global

mu.bar <- 0.105
W <- ((C - mu.bar * B) / D) * Si + ((mu.bar * A - B) / D) * Su; W
s.W <- sqrt(t(W) %*% S %*% W); s.W

w.g <- Si / A; w.g
s.g <- 1 / sqrt(A); s.g
mu.g <- t(w.g) %*% mu; mu.g


# 4. Portafolio tangente

R.f <- 4.5 / 365

w.tg <- solve(S) %*% (mu - R.f * i) / (c(t(mu - R.f * i) %*% Si)); w.tg
mu.tg <- c(t(w.tg) %*% mu); mu.tg
s.tg <- c(sqrt(t(w.tg) %*% S %*% w.tg)); s.tg
sharpe <- (mu.tg - R.f) / s.tg; sharpe


# 5. Línea de comando

# 5.1 Estadísticas básicas
Datos <- as.timeSeries(Datos)
Datos <- portfolioData(data = Datos, spec = portfolioSpec())
getStatistics(Datos)

# 5.2 Frontera óptima y eficiente
frontera <- portfolioFrontier(Datos)
frontierPlot(frontera, cex = 0.6, pch = 19, 
             col = c("blue", "gray"))

monteCarloPoints(frontera, mcSteps = 1000,
                 pch = 19, col = "gray82", cex = 0.2)
singleAssetPoints(frontera, 
                  col = "green4", pch = 19, cex = 1)
equalWeightsPoints(frontera,
                   cex = 1, col = "magenta", pch = 19)

# 5.3 Mínimo global
min.var <- efficientPortfolio(data = Datos,
                              constraints = "LongOnly")

min.var
minvariancePoints(min.var, col = "red4", pch = 19, cex = 1)
weightsPie(min.var)

# 5.4 Retorno esperado 0.105
esp.cartera <- portfolioSpec()
setTargetReturn(esp.cartera) = mu.bar
por.efi <- efficientPortfolio(data = Datos, esp.cartera,
                              constraints = "LongOnly")
por.efi
weightsPie(por.efi)

# 5.5 Portafolio tangente
R.f <- 4.5 / 365
tg.p <- portfolioSpec()
setRiskFreeRate(tg.p) <- R.f

tgPortafolio <- tangencyPortfolio(Datos, spec = tg.p,
                                  constraints = "LongOnly")
tgPortafolio
weightsPie(tgPortafolio)

# 5.5.1 Ratio de Sharpe
mu.tan <- getTargetReturn(tgPortafolio)[1]
s.tan <- getTargetRisk(tgPortafolio)[1]
(mu.tan - R.f) / s.tan

# 5.5.2 Gráfica
frontierPlot(frontera, pch = 19, cex = 0.6,
             col = c("blue4", "green3"))
grid()
monteCarloPoints(frontera, mcSteps = 1000, return = "mu",
                 pch = 19, col = "gray82", cex = 0.2)
minvariancePoints(min.var, col = "red4", pch = 19, cex = 1)
tangencyLines(tgPortafolio, return = "mu", col = "red4")
sharpeRatioLines(frontera, return = "mu", col = "steelblue")

# 6. General
plot(frontera)
