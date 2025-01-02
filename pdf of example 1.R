rm(list=ls())
library(fitdistrplus)
library(MASS)
library(survival)


x <- c(0.1, 0.2, 1, 1, 1, 1, 1, 2, 3, 6, 7, 11, 12, 18, 18, 18, 18, 18, 21, 32, 36, 40, 45, 46, 47, 50, 55, 60, 63, 63, 67, 67, 67, 67, 72, 75, 79, 82, 82, 83, 84, 84, 84, 85, 85, 85, 85, 85, 86, 86)

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
a <- 0.0205
b <- 0.3444
 
chenpdf<-dchenpdf(x,a,b)

# Define Weibull Chen PDF
dweibull_chen <- function(x, alpha, beta, lemda, theta) {
  ((alpha * beta * x^(beta-1)) * (exp(x^beta)) + lemda * theta * x^(theta-1)) * exp(alpha*(1-exp(x^beta))-lemda*x^theta)
}

# Calculate PDF of Weibull Chen distribution
wcpdf <- dweibull_chen(x, 1 * 10^-10, 0.7080, 0.1014, 0.5042)

# Define Additive weibull distribution
daw<- function(x,a,b,c,d){(a*b*x^(b-1)+ c*d*x^(d-1))*exp(-a*x^(b)-c*x^(d))}

# Calculate PDF of Additive weibull distribution
a<- 3*10^(-6)
b<- 2.9379
c<- 0.1038
d<- 0.3666
  
awpdf<-daw(x,a,b,c,d)

# Define Marshall-Olkin extended Chen distribution
moec<- function(x, a, b, c, d) {num <- b * c * (d - 1) * (x / a) ^ (b - 1) * exp((x / a) ^ (b) + a * c * (1 - exp((x / a) ^ b)))
  den <- (1 - (1 - d) * exp(a * c * (1 - exp(x / a) ^ (b)))) * log(d)
	num / den}

# Calculate PDF of Marshall-Olkin extended Chen distribution
a=12.480
b=0.4794
c= 0.0356
d=111.83
dmoecpdf<-moec(x,a,b,c,d)

#Define Exponentiated modified Weibull extension
emwe <- function(x, a, b, c, d) {
  c * b * d * (x / a) ^ (b - 1) * exp((x / a) ^ b + a * c * (1 - exp(x / a) ^ (b))) * (1 - exp(a * c * (1 - exp(x / a) ^ (b)))) ^ (d - 1)
}

#Calculate PDF of Exponentiated modified Weibull extension
a <- 49.521
b <- 3.1338
c <- 8e-5
d <- 0.1561
   
emwepdf<- emwe(x,a,b,c,d)


# Define colors for each distribution
weibull_color <- "orange"
chen_color <- "green"
weibull_chen_color <- "blue"
additive_weibull_color <- "purple"
moec_color <- "pink"
emwe_color <- "brown"

# Set up square plot
par(mfrow=c(2,3))

# Plot the Weibull PDF
hist(x, breaks = 9, probability = TRUE, main = "Empirical PDF vs Weibull PDF", xlab = "y", ylab = "Probability", xlim = c(0, 80), ylim = c(0, 0.030), col = "gray")
lines(x, wpdf, type = "s", col = weibull_color, lwd = 2)

# Plot the Chen PDF
hist(x, breaks = 9, probability = TRUE, main = "Empirical PDF vs Chen PDF", xlab = "y", ylab = "Probability", xlim = c(0, 80), ylim = c(0, 0.030), col = "gray")
lines(x, chenpdf, type = "s", col = chen_color, lwd = 2, lty = 2)

# Plot the Weibull Chen PDF
hist(x, breaks = 9, probability = TRUE, main = "Empirical PDF vs Weibull Chen PDF", xlab = "y", ylab = "Probability", xlim = c(0, 80), ylim = c(0, 0.030), col = "gray")
lines(x, wcpdf, type = "s", col = weibull_chen_color, lwd = 2, lty = 3)

# Plot the Additive Weibull PDF
hist(x, breaks = 9, probability = TRUE, main = "Empirical PDF vs Additive Weibull PDF", xlab = "y", ylab = "Probability", xlim = c(0, 80), ylim = c(0, 0.030), col = "gray")
lines(x, awpdf, type = "s", col = additive_weibull_color, lwd = 2, lty = 4)

# Plot the Marshall-Olkin extended Chen PDF
hist(x, breaks = 9, probability = TRUE, main = "Empirical PDF vs MOEC PDF", xlab = "y", ylab = "Probability", xlim = c(0, 80), ylim = c(0, 0.030), col = "gray")
lines(x, dmoecpdf, type = "s", col = moec_color, lwd = 2, lty = 5)

# Plot the Exponentiated modified Weibull extension PDF
hist(x, breaks = 9, probability = TRUE, main = "Empirical PDF vs EMWE PDF", xlab = "y", ylab = "Probability", xlim = c(0, 80), ylim = c(0, 0.030), col = "gray")
lines(x, emwepdf, type = "s", col = emwe_color, lwd = 2, lty = 6)

# Add main title to the plot
mtext("Empirical PDF vs Fitted PDFs", outer = TRUE, font = 2, cex = 1.5, line = 1)

