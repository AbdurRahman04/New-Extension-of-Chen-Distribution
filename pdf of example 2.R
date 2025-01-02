rm(list=ls())
library(fitdistrplus)

x <- c(0.0094, 0.05, 0.4064, 4.6307, 5.1741, 5.8808, 6.3348, 7.1645, 7.2316, 8.2604,
       9.2662, 9.3812, 9.5223, 9.8783, 9.9346, 10.0192, 10.4077, 10.4791, 11.076, 11.325,
       11.5284, 11.9226, 12.0294, 12.074, 12.1835, 12.3549, 12.5381, 12.8049, 13.4615,
       13.853)

# Fit Weibull distribution
fit <- fitdistr(x, "weibull")

# Calculate empirical PDF
emp_pdf <- density(x)

# Calculate PDF of Weibull distribution
wpdf <- dweibull(x, shape = fit$estimate[1], scale = fit$estimate[2])

# Define Chen distribution
dchenpdf<- function(x, a, b) {
  ((a*b*x^(b-1))*exp(a*(1-exp(x^b))+ x^b))
}
# Calculate PDF of chen distribution
a <- 0.0064
b <- 0.6886
chenpdf<-dchenpdf(x,a,b)

# Define Weibull Chen PDF
dweibull_chen <- function(x, alpha, beta, lemda, theta) {
  ((alpha * beta * x^(beta-1)) * (exp(x^beta)) + lemda * theta * x^(theta-1)) * exp(alpha*(1-exp(x^beta))-lemda*x^theta)
}

# Calculate PDF of Weibull Chen distribution
wcpdf <- dweibull_chen(x, .0008, .8125, 0.0814, 0.2763)

# Define Additive weibull distribution
daw<- function(x,a,b,c,d){(a*b*x^(b-1)+ c*d*x^(d-1))*exp(-a*x^(b)-c*x^(d))}

# Calculate PDF of Additive weibull distribution
a<- 6*10^(-9)
b<- 7.6910
c<- 0.1432
d<- 0.2266
awpdf<-daw(x,a,b,c,d)

# Define Marshall-Olkin extended Chen distribution
moec<- function(x, a, b, c, d) {num <- b * c * (d - 1) * (x / a) ^ (b - 1) * exp((x / a) ^ (b) + a * c * (1 - exp((x / a) ^ b)))
  den <- (1 - (1 - d) * exp(a * c * (1 - exp(x / a) ^ (b)))) * log(d)
	num / den}

# Calculate PDF of Marshall-Olkin extended Chen distribution
a=0.0987
b=0.4974
c=0.0002
d=0.3718
dmoecpdf<-moec(x,a,b,c,d)

#Define Exponentiated modified Weibull extension
emwe <- function(x, a, b, c, d) {
  c * b * d * (x / a) ^ (b - 1) * exp((x / a) ^ b + a * c * (1 - exp(x / a) ^ (b))) * (1 - exp(a * c * (1 - exp(x / a) ^ (b)))) ^ (d - 1)
}

#Calculate PDF of Exponentiated modified Weibull extension
a <- 3.2124
b <- 1.0219
c <- 0.0114
d <- 1.0773
emwepdf<- emwe(x,a,b,c,d)


# Define colors for each distribution
weibull_color <- "orange"
chen_color <- "green"
weibull_chen_color <- "blue"
additive_weibull_color <- "purple"
moec_color <- "pink"
emwe_color <- "brown"

# Set up square plot
par(mfrow=c(2,3), mar=c(4, 4, 2, 1), mgp=c(2.5,1,0), oma=c(0,0,2,0))

# Plot the Weibull PDF
hist(x, breaks =6 , probability=T, main = "Empirical PDF vs WeibulL PDF", xlab = "y", ylab = "Probability", xlim = c(0, 14), ylim = c(0, 0.20), col = "gray")
lines(x, wpdf, type = "s", col = weibull_color, lwd = 3)

# Plot the Chen PDF
hist(x, breaks =6 , probability=T, main = "Empirical PDF vs Chen PDF", xlab = "y", ylab = "Probability", xlim = c(0, 14), ylim = c(0, 0.20), col = "gray")
lines(x, chenpdf, type = "s", col = chen_color, lwd = 3, lty = 2)

# Plot the Weibull Chen PDF
hist(x, breaks =6 , probability=T, main = "Empirical PDF vs Weibull Chen PDF", xlab = "y", ylab = "Probability", xlim = c(0, 14), ylim = c(0, 0.20), col = "gray")
lines(x, wcpdf, type = "s", col = weibull_chen_color, lwd = 3, lty = 3)

# Plot the Additive Weibull PDF
hist(x, breaks =6 , probability=T, main = "Empirical PDF vs Additive Weibull PDF", xlab = "y", ylab = "Probability", xlim = c(0, 14), ylim = c(0, 0.20), col = "gray")
lines(x, awpdf, type = "s", col = additive_weibull_color, lwd = 3, lty = 4)

# Plot the Marshall-Olkin extended Chen PDF
hist(x, breaks =6 , probability=T, main = "Empirical PDF vs MOEC PDF", xlab = "y", ylab = "Probability", xlim = c(0, 14), ylim = c(0, 0.20), col = "gray")
lines(x, dmoecpdf, type = "s", col = moec_color, lwd = 3, lty = 5)

# Plot the Exponentiated modified Weibull extension PDF
hist(x, breaks =6 , probability=T, main = "Empirical PDF vs EMWE PDF", xlab = "y", ylab = "Probability", xlim = c(0, 14), ylim = c(0, 0.20), col = "gray")
lines(x, emwepdf, type = "s", col = emwe_color, lwd = 3, lty = 6)

# Add main title to the plot
mtext("Empirical PDF vs Fitted PDFs", outer=TRUE, font=2, cex=1.5, line=1)

