# =========================================================
#     Portafolio con 3 activos (2025/10/07)
# =========================================================

library(ggplot2)

# 1. Datos

i <- matrix(c(1, 1, 1), 3); i
mu <- matrix(c(4, 8, 6), 3); mu
S <- matrix(c(2, 1, 0, 1, 4, 1, 0, 1, 3), 3); S


# 2. Simulación de Portafolios

P <- matrix(runif(9000, min = 0, max = 100), ncol = 3)
W = P / rowSums(P)

risk <- c()
for (j in 1 : 3000){
  risk[j] <- sqrt(W[j, ] %*% S %*% t(W)[ ,j])
}
mu.p <- mu[1,1] * W[ ,1] + mu[2,1] * W[ ,2] + mu[3,1] * W[ ,3]

p <- ggplot(data.frame(risk, mu.p), aes(x = risk, y = mu.p)) +
  geom_point(size = 0.5, colour = "grey50") +
  labs(title = "Simulación de Portafolios",
       x = expression(sigma[p]),
       y = expression(mu[p]))

p.1 <- p + 
  annotate('point', x = sqrt(S[1,1]), y = mu[1,1], size = 2, colour = "black") +
  annotate('text', x = sqrt(S[1,1]), y = mu[1,1], label = "1", size = 4,
           hjust = 0.5, vjust = 1.5) +
  annotate('point', x = sqrt(S[2,2]), y = mu[2,1], size = 2, colour = "black") +
  annotate('text', x = sqrt(S[2,2]), y = mu[2,1], label = "2", size = 4,
           hjust = 0.5, vjust = 1.5) +  # arriba
  annotate('point', x = sqrt(S[3,3]), y = mu[3,1], size = 2, colour = "black") +
  annotate('text', x = sqrt(S[3,3]), y = mu[3,1], label = "3", size = 4,
           hjust = 0.5, vjust = 1.5)

p.1

# 2. Portafolios equitativo

W.eq <- matrix(c(rep(1,3)/ 3), 3); W.eq
retorno <- c( t(W.eq) %*% mu ); retorno
s.eq <- sqrt(t(W.eq) %*% S %*% W.eq); s.eq


# 3. Portafolio mínimo global y retorno dado

A <- c(t(i) %*% solve(S) %*% i); A
B <- c(t(i) %*% solve(S) %*% mu); B
C <- c(t(mu) %*% solve(S) %*% mu); C
D <- A * C - B^2; D

Si <- solve(S) %*% i; Si
Su <- solve(S) %*% mu; Su

# Mínimo global
w.g <- Si / A; w.g
s.g <- 1 / sqrt(A); s.g
mu.g <- t(w.g) %*% mu; mu.g


# Mínimo retorno 5.5

mu.bar <- 5.5
W <- ((C - mu.bar * B) / D) * Si + ((mu.bar * A - B) / D) * Su; W
a <- sqrt((mu.bar^2 * A - 2 * mu.bar * B + C) / D); a

sqrt(t(W) %*% S %*% W)


# 4. Portafolio mínimo global y retorno dado

p.1 + 
  annotate('point', x = s.g, y = mu.g, size = 2.5, colour = "red") +
  annotate('text', x = s.g, y = mu.g, label = "mínimo global",
           size = 4.5, colour = "red", hjust = -0.2, vjust = 0.5) +
  annotate('point', x = a, y = mu.bar, size = 2.5, colour = "blue") +
  annotate('text', x = a, y = mu.bar, label = "óptimo",
           size = 4.5, colour = "blue", hjust = -0.25, vjust = 0.5) +
  annotate('segment', x = a, y = 3, xend = a, yend = 5.5,
           lwd = 0.6, colour = "black", linetype = "dashed") +
  ggtitle("Mínimo global")
