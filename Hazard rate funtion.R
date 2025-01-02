rm(list=ls())
x <-sort.default( c(0.0094, 0.05, 0.4064, 0.5, 1, 1.5, 2.0, 0.3, .10, 1.3,1.5, 1.9, .6, .7, 1.8))

# probability distribution of weibull-chen distribution

wc <- function(x, alpha, beta, lemda, theta) {
  ((alpha * beta * x^(beta-1)) * (exp(x^beta)) + lemda * theta * x^(theta-1)) * exp(alpha*(1-exp(x^beta))-lemda*x^theta)
}

pwc1<-wc(x,.25,1,.75,1.25)
pwc2<-wc(x,0,.25,1,.5)
pwc3<-wc(x,.25,1.25,.25,.5)
pwc4<-wc(x,.5,.5,.25,1.5)
pwc5<-wc(x,.25,.25,.25,2.5)

# First, combine all the values into a data frame
pdf <- data.frame(pwc1, pwc2, pwc3, pwc4, pwc5)

#par(mfrow = c(1, 2))
# Then, plot the data using the plot() function
plot(x, pdf$pwc1, type="l", col="red", xlab="X", ylab="PDF",lwd=2,main = "Weibull-Chen PDF")
lines(x, pdf$pwc2, type="l", col="blue",lwd=1,lty=4)
lines(x, pdf$pwc3, type="l", col="green",lwd=1,lty=5)
lines(x, pdf$pwc4, type="l", col="gray",lwd=1,lty=6)
lines(x, pdf$pwc5, type="l", col="orange",lwd=1,lty=7)
legend("topright", c("pdf1", "pdf2", "pdf3", "pdf4", "pdf5"), col=c("red", "blue", "green", "gray", "orange"), pch=16,cex = .5)

hx<- function(x, alpha, beta, lemda, theta) {
  ((alpha * beta * x^(beta-1)) * (exp(x^beta)) + lemda * theta * x^(theta-1))}

phx1<-hx(x,.25,1,.75,1.25)
phx2<-hx(x,0,.25,1,.5)
phx3<-hx(x,.25,1.25,.25,.5)
phx4<-hx(x,.5,.5,.25,1.5)
phx5<-hx(x,.25,.25,.25,2.5)
# First, combine all the values into a data frame
df <- data.frame(phx1, phx2, phx3, phx4, phx5)

# Then, plot the data using the plot() function
plot(x, df$phx1, type="l", col="red", xlab="X", ylab="Hazard Rate",lwd=2,main = "Weibull-Chen Hazard Rate")
lines(x, df$phx2, type="l", col="blue",lwd=2,lty=4)
lines(x, df$phx3, type="l", col="green",lwd=2,lty=5)
lines(x, df$phx4, type="l", col="gray",lwd=2,lty=6)
lines(x, df$phx5, type="l", col="orange",lwd=2,lty=7)
legend("top", c(".25,1,.75,1.25)", "0,.25,1,.5", ".25,1.25,.25,.5", ".5,.5,.25,1.5", ".25,.25,.25,2.5"), col=c("red", "blue", "green", "gray", "orange"), pch=16, cex=.7)


