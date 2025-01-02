rm(list=ls())
library(fitdistrplus)


x <- c(0.0094, 0.05, 0.4064, 4.6307, 5.1741, 5.8808, 6.3348, 7.1645, 7.2316, 8.2604,
       9.2662, 9.3812, 9.5223, 9.8783, 9.9346, 10.0192, 10.4077, 10.4791, 11.076, 11.325,
       11.5284, 11.9226, 12.0294, 12.074, 12.1835, 12.3549, 12.5381, 12.8049, 13.4615,
       13.853)

# Weibull distribution
fit <- fitdistr(x, "weibull")

# cumulative probabilities
wcdf <- pweibull(x, shape = fit$estimate[1], scale = fit$estimate[2])

# Define Chen CDF
pchencdf <- function(x, a, b) {
  (1- exp(a*(1-exp(x^b))))}

# Calculate CDF of chen distribution
a <- 0.0064
b <- 0.6886
chencdf<-pchencdf(x,a,b)

# Weibull chen CDF
pweibull_chen <- function(x, alpha, beta, lemda, theta) {
  1 - exp(alpha*(1-exp(x^beta)) - lemda*(x^theta))
}

# Weibull chen distribution
wccdf <- pweibull_chen(x,.0008,.8125,0.0814,0.2763)

#Define Additive weibull ddistribution
paw<- function(x,a,b,c,d){1-exp(-a*x^(b)-c*x^(d))}

#Calculate CDF of Additive weibull ddistribution
a<- 6*10^(-9)
b<- 7.6910
c<- 0.1432
d<- 0.2266
awcdf<-paw(x,a,b,c,d)


# Define Marshall-Olkin extended Chen distribution
library(pracma)

mo_ec_pdf <- function(x, a, b, c, d) {
  (b * c * (d - 1) * (x / a) ^ (b - 1) * exp((x / a) ^ b + a * c * (1 - exp((x / a) ^ b))))/((1-(1-d)*exp(a*c*(1-exp(x/a)^(b))))*(log(d)))
}
mo_ec_cdf <- function(x) {
  integrate(function(t) mo_ec_pdf(t, a, b, c, d), lower = 0, upper = x)$value
}


# Calculate PDF of Marshall-Olkin extended Chen distribution
a=0.0987
b=0.4974
c=0.0002
d=0.3718
mo_ec_cdf_values <- sapply(x, mo_ec_cdf)


#Define Exponentiated modified Weibull extension
emwe <- function(x, a, b, c, d) {
  c * b * d * (x / a) ^ (b - 1) * exp((x / a) ^ b + a * c * (1 - exp(x / a) ^ (b))) * (1 - exp(a * c * (1 - exp(x / a) ^ (b)))) ^ (d - 1)
}
emwe_cdf <- function(x, a, b, c, d) {
  integrate(Vectorize(function(t) emwe(t, a, b, c, d)), 0, x)$value
}

#Calculate PDF of Exponentiated modified Weibull extension
a <- 3.2124
b <- 1.0219
c <- 0.0114
d <- 1.0773
emwecdf <- sapply(x, function(t) emwe_cdf(t, a, b, c, d))


# plot the CDFs
par(mfrow=c(2,3), mar=c(4, 4, 2, 1), mgp=c(2.5,1,0), oma=c(0,0,2,0))

# Weibull distribution in orange
weibull_color <- "orange"
plot(ecdf(x), main = "Empirical CDF vs Weibull CDF")
lines(x, wcdf, type = "s", xlab = "y", ylab = "Probability", 
     main = "CDF of Weibull Distribution", col = weibull_color, lwd = 3)

# Chen distribution in green
chen_color <- "green"
plot(ecdf(x), main = "Empirical CDF vs Chen CDF")
lines(x,chencdf, lwd = 3, col = chen_color)

# Weibull Chen distribution in blue
weibull_chen_color <- "blue"
plot(ecdf(x), main = "Empirical CDF vs Weibull Chen CDF")
lines(x, wccdf, lty = 5, col = weibull_chen_color, lwd = 3, xlim = c(0, 14), ylim = c(0, 1))

# Additive Weibull distribution in purple
additive_weibull_color <- "purple"
plot(ecdf(x), main = "Empirical CDF vs Additive Weibull")
lines(x,awcdf, xlim=c(0,14), ylim=c(0,1), lwd = 3, col=additive_weibull_color)

# Marshall-Olkin extended Chen (MOEC) distribution in pink
moec_color <- "pink"
plot(ecdf(x), main = "Empirical CDF vs MOEC CDF")
lines(x, mo_ec_cdf_values,  lwd = 3, col = moec_color)

# Exponentiated modified Weibull extension (EMWE) distribution in brown
emwe_color <- "brown"
plot(ecdf(x), main = "Empirical CDF vs EMWE")
lines(x, emwecdf,  lwd = 3, col = emwe_color)

# Add main title to the plot
mtext("Empirical CDF vs Fitted CDFs", outer=TRUE, font=2, cex=1.5, line=1)

