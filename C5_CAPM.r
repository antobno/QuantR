

library(ggplot2)

load("~/Documents/RBases/CAPM.RData")

r.apple <- (diff(log(CAPM$AAPL))) * 100
r.sp <- (diff(log(CAPM$SP))) * 100

p <- ggplot(data.frame(r.apple, r.sp)) +
  geom_point(aes(x = r.sp, y = r.apple), colour = "red4") +
  labs(title = "", x = expression(mu[p]), 
       y = expression(mu[i])")
       
p
  

beta <- cov(r.apple, r.sp) / var(r.sp)
alpha <- mean(r.apple) - beta * mean(r.sp)

alpha
beta

ols <- lm(r.apple ~ r.sp)
summary(ols)

p + geom_abline(intercept = ols$coef[1], slope = ols$coef[2],
                colour = "blue4", lwd = 1)
