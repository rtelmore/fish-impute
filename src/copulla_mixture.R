#Mean 0,0
mu <- c(0, 0)
#correlation 0.5
sigma <- matrix(c(1, rho, rho, 1), nrow = 2)
dat <- as.data.frame(mvtnorm::rmvnorm(n=n, mean=mu, sigma = sigma))

#pdf of normal mixture
f_mix2 <-function(x, p = c(0.5, 0.5), mu = c(0, 5), sigma = c(1,1)){
  p[1]*dnorm(x, mu[1], sigma[1]) + p[2]*dnorm(x,mu[2], sigma[2])
}

plot(x, f_mix2(x), type = "l")

#Find cdf of mixture 
F_mix2 <- function(t){
  integrate(f_mix2,lower = -Inf,upper = t)$value
}

F_mix2 <- Vectorize(F_mix2)
plot(x, F_mix2(x), type = "l")

Finv_mix2 <- function(p){
  uniroot(function(x){F_mix2(x) - p},lower = -10, upper = 10)$root
}
Finv_mix2(0.7)
Finv_mix2 <- Vectorize(Finv_mix2)
y <- Finv_mix2(pnorm(dat$V1))
hist(y)

