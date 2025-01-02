rm(list=ls())
options(scipen = 999)
weibull_chen_rnd <- function(n, alpha, beta, lambda, theta) {
  # Define the PDF of the Weibull-Chen distribution
  f <- function(x) (alpha * beta * x^(beta-1)) * (exp(x^beta)) + lambda * theta * x^(theta-1) * exp(alpha*(1-exp(x^beta))-lambda*x^theta)
   
  # Compute the CDF of the Weibull-Chen distribution using the trapezoidal rule
  x <- seq(0, 10, length.out = 10000)
  dx <- x[2] - x[1]
  F <- cumsum(f(x)) * dx
   
  # Generate random numbers using the inverse transform method
  U <- runif(n)
  X <- approx(F, x, xout = U)$y
  
  return(X)
}

set.seed(123)  # Set the random seed for reproducibility
x<-weibull_chen_rnd(1000, .25,1 , 0.5, 1.5)  # Generate 1000 random numbers


logL <- function(x, par) {
  alpha <- par[1]
  beta <- par[2]
  lambda <- par[3]
  theta <- par[4]
  sum(log((alpha * beta * x^(beta-1)) * (exp(x^beta)) + lambda * theta * x^(theta-1) * exp(alpha*(1-exp(x^beta))-lambda*x^theta)))
}


logL(x,par=c(0.25,1,0.75,.5))
est <- nlm(logL, x, p = c(0.25, 1, 0.75, 0.5))
alpha <- est$estimate[1]
beta <- est$estimate[2]
lambda <- est$estimate[3]
theta <- est$estimate[4]
print(c(alpha, beta, lambda, theta))

