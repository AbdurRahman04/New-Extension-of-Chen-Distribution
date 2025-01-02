rm(list=ls())
library(fitdistrplus)

rm(list=ls())

x <- c(0.1, 0.2, 1, 1, 1, 1, 1, 2, 3, 6, 7, 11, 12, 18, 18, 18, 18, 18, 21, 32, 36, 40, 45, 46, 47, 50, 55, 60, 63, 63, 67, 67, 67, 67, 72, 75, 79, 82, 82, 83, 84, 84, 84, 85, 85, 85, 85, 85, 86, 86)


# Weibull distribution
fit <- fitdistr(x, "weibull")

# cumulative probabilities
wcdf <- pweibull(x, shape = fit$estimate[1], scale = fit$estimate[2])

# Define Weibull Chen CDF
pchencdf <- function(x, a, b) {
  (1- exp(a*(1-exp(x^b))))}

# Calculate CDF of chen distribution
a <- 0.0205
b <- 0.3444
chencdf<-pchencdf(x,a,b)

# Weibull chen CDF
pweibull_chen <- function(x, alpha, beta, lemda, theta) {
  1 - exp(alpha*(1-exp(x^beta)) - lemda*(x^theta))
}

# Weibull chen distribution
wccdf <- pweibull_chen(x,1 * 10^-10, 0.7080, 0.1014, 0.5042)

#Define Additive weibull ddistribution
paw<- function(x,a,b,c,d){1-exp(-a*x^(b)-c*x^(d))}

#Calculate CDF of Additive weibull ddistribution
a<- 3*10^(-6)
b<- 2.9379
c<- 0.1038
d<- 0.3666
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
a=12.480
b=0.4794
c= 0.0356
d=111.83
mo_ec_cdf_values <- sapply(x, mo_ec_cdf)


#Define Exponentiated modified Weibull extension
emwe <- function(x, a, b, c, d) {
  c * b * d * (x / a) ^ (b - 1) * exp((x / a) ^ b + a * c * (1 - exp(x / a) ^ (b))) * (1 - exp(a * c * (1 - exp(x / a) ^ (b)))) ^ (d - 1)
}
emwe_cdf <- function(x, a, b, c, d) {
  integrate(Vectorize(function(t) emwe(t, a, b, c, d)), 0, x)$value
}

#Calculate PDF of Exponentiated modified Weibull extension
a <- 49.521
b <- 3.1338
c <- 8e-5
d <- 0.1561
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

