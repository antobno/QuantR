# =========================================================
#     Simulación de portafolios (2025/09/25)
# =========================================================

library(ggplot2)


A <- matrix(runif(2000, min = 0, max = 100), ncol = 2)
W <- A / rowSums(A)
S <- matrix(c(100, -20, -20, 16), 2, 2)
mu <- matrix(c(16, 10), 2); mu

risk <- c()
for (i in 1 : 1000){
  risk[i] <- sqrt(W[i, ] %*% S %*% t(W)[ ,i])
}

mu.p <- mu[1,1] * W[ ,1] + mu[2,1] * W[ ,2]

p <- ggplot(cbind(risk, mu.p), aes(x = risk, y = mu.p)) +
  geom_point(size = 0.5, colour = "blue4") +
  annotate('point', x = 10, y = 16, size = 2, colour = "red4") +
  annotate('point', x = 4, y = 10, size = 2, colour = "red4") +
  annotate('text', x = 10, y = 15.7, label = "1", size = 4) +
  annotate('text', x = 4, y = 9.7, label = "2", size = 4) +
  labs(x = expression(sigma[p]), y = expression(mu[p])) + 
  ggtitle("Simulación de portafolios")

p

min(risk)
which.min(risk)
W[which.min(risk), ]

pp <- p + annotate('point', x = min(risk), y = mu.p[which.min(risk)],
             size = 2, colour = "red4")


print(pp) # Para que me muestre todo al dar "source"
