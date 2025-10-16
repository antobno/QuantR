# =========================================================
#     Portafolio con 4 activos obtenidos de Yahoo Finance (2025/10/14)
# =========================================================

library(quantmod)
library(ggplot2)
library(dbplyr)
library(conflicted)
library(tidyverse)


#1. Precios de activos de Apple, Microsoft, Intel y Amazon

precios <- c("AAPL", "MSFT", "INTC", "AMZN")
getSymbols(precios, from = "2019-01-02", periodicity = "daily")

plot(AAPL$AAPL.Close)
plot(MSFT$MSFT.Close)
plot(INTC$INTC.Close)
plot(AMZN$AMZN.Close)


# 2. Retornos

R.ap <- diff(log(AAPL$AAPL.Close)) * 100
R.mi <- diff(log(MSFT$MSFT.Close)) * 100
R.in <- diff(log(INTC$INTC.Close)) * 100
R.am <- diff(log(AMZN$AMZN.Close)) * 100

plot(R.ap)
plot(R.mi)
plot(R.in)
plot(R.am)


# 3. Media y varianza

mu <- rbind(mean(R.ap[-1]), mean(R.mi[-1]), mean(R.in[-1]), mean(R.am[-1])); mu

Dat <- data.frame(R.ap[-1], R.mi[-1], R.in[-1], R.am[-1])
S <- cov(Dat); S


# 4. Matrices requeridas

i <- matrix(c(1, 1, 1, 1), 4); i
Si <- solve(S) %*% i; Si
Su <- solve(S) %*% mu; Su

A <- c(t(i) %*% solve(S) %*% i)
B <- c(t(i) %*% solve(S) %*% mu)
C <- c(t(mu) %*% solve(S) %*% mu)
D <- A * C - B^2


# 5. Frontera óptima

x <- seq(0, 3.5, length = 100)
y <- seq(0, 0.3, length = 100)

df <- expand.grid(x = x, y = y) %>%
  mutate(z = D * x^2 - A * y^2 + 2 * B * y - C)

p <- ggplot(df, aes(x = x, y = y, z = z)) +
  geom_contour(bins = 0, lwd = 1, colour = "blue4") +
  labs(x = expression(sigma[p]),
       y = expression(mu[p]))

p + ggtitle("Portafolios óptimos")


# 6. Portafolio con retorno 0.1 y mínimo global

mu.bar <- 0.1

W <- ((C - mu.bar * B) / D) * Si + ((mu.bar * A - B) / D) * Su; W
a <- sqrt((mu.bar^2 * A - 2 * mu.bar * B + C)/ D); a
sqrt(t(W) %*% S %*% W);

w.g <- Si / A; w.g
s.g <- 1 / sqrt(A); s.g
mu.g <- t(w.g) %*% mu

p +
  geom_point(aes(x = a, y = mu.bar), colour = "green") +
  geom_point(aes(x = c(s.g), y = c(mu.g)), colour = "red4") +
  ggtitle("Portafolios óptimos")
