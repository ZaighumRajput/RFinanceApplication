#Time Series Concepts

#Gaussian White Noise Process
set.seed(runif(1, -100,100)) #random seed; seed is from draw of standard normal
y=rnorm(250) # returns a vector of 250 draws from the standard normal
ts.plot(y, main="Gaussian White Noise Process", xlab="time",
        ylab="y(t)", col="blue", lwd=2)
abline(h=0)

#GWN on Monthly Continous Compounded Returns

y=rnorm(60, mean=0.01, sd=0.05)
ts.plot(y, main="GWN for Monthly Continuous Compounded Returns",
        xlab="time", ylab="r(t)", col="blue", lwd=2, type="h")

abline(h=c(0, -0.05, 0.05), lwd=2, lty=c("solid", "dotted", "dotted"),
       col=c("black", "red", "red"))

#A deterministic trend with some noise
y = rnorm(250)
y.dt = 0.1*seq(1,250) + y
ts.plot(y.dt, lwd=2, col=2)

#Random Walk
z = rnorm(250); y.rw = cumsum(z); 
ts.plot(y.rw, lwd=2, col="green", main="Random Walk assuming Gaussian Steps"); abline(h = 0, col = "black", lty = "dotted")


