#Markowitz Analysis
####SAMPLE DATA
mu = c(4.27, 0.15, 2.85)
names(mu) <- c("MSFT", "NRDS", "STBX")
#Matrix of covariances
Omega = matrix(c(1, 0.18, 0.11, 0.18, 1.1, 0.26, 0.11, 0.26, 1.99), nrow=3, ncol=3)
rownames(Omega) <- c("MSFT", "NRDS", "STBX")
colnames(Omega) <- c("MSFT", "NRDS", "STBX")
######


#Markowitz Analysis Function
# Omega Inverse
# = means its default value
markowitz <-function( Mu = mu, omega = Omega){
OmegaInv <- try(solve(omega), TRUE)
if("try-error" %in% class(OmegaInv)) stop

#1
One = rep.int(1,length(mu))
#a
a = as.double(t(Mu)%*%OmegaInv%*%Mu)
#b
b = as.double(t(Mu)%*%OmegaInv%*%One)
#c
c = as.double(t(One)%*% OmegaInv %*% One)
#d
d = a*c - b^2

Phi = (a/d)*(OmegaInv %*% One) - (b/d)*(OmegaInv %*% mu)
Theta = (c/d)*(OmegaInv %*% mu) - (b/d)*(OmegaInv %*% One)

####
mup = b/c
weights = Phi + Theta*mup
var = (c/d)*((mup-(b/c))^2) + (1/c)
sigma = var^(0.5)
Answer = return(list(weights = weights, mup = mup, var = var,a=a, b = b, c=c, d=d, Phi=Phi,Theta=Theta))
  #data.frame(c(weights), mup, var, b, c, d)

return(Answer)
}

MVPequation <- function(up, b=markowitz()$b, c = markowitz()$c, d = markowitz()$d)
{
  
  sigmasqrd = sqrt((c/d)*( up - b/c)^2 + 1/c)
  return ( sigmasqrd )
}

samplemu = seq(0,6,0.1)

samplesig = lapply(samplemu, MVPequation) 

plot(samplesig, samplemu, xlab=expression(sigma), ylab =expression(mu), type = "l", xlim = c(0.6,1), main ="Markowitz Hyperbola Frontier")
points(sqrt(markowitz()$var), markowitz()$mup, type="o" ,col="purple", pch="M"  )


### Q@

####
#Q2# Tangency Portfolio
####
#N risky asset plus rf. 
#
#
#$tangentWeights
#MSFT       NRDS       STBX 
#0.9171983 -0.1964281  0.2792298 

#$mupTangent
#[,1]
#[1,] 4.682777

#$sigmaTangent
#[,1]
#[1,] 1.000908

#$h
#[1] 21.42368

#Equation for efficient frontier
# up = 0.05 = sqrt(21.42)sig_p

TangencyPortfolio <- function(rf = 0.05, Omega = Omega, Mu = mu) {
One = rep.int(1, length(Mu))
a = markowitz(Mu, Omega)$a
b = markowitz(Mu, Omega)$b
c = markowitz(Mu, Omega)$c
d = markowitz(Mu, Omega)$d


mu.minus.rf = Mu - rf*One

OmegaInv <- try(solve(Omega), TRUE)
if("try-error" %in% class(OmegaInv)) stop

#numerator
top.matrix = OmegaInv %*% mu.minus.rf
#denominator
bot.value = as.numeric(t(One)%*%top.matrix)
#
w.tangent.vector = top.matrix[,1]/bot.value


mup.tangent = t(w.tangent.vector)%*%Mu


sigma.tangent = (t(w.tangent.vector)%*%Omega%*%(w.tangent.vector))^0.5
## to characterize the equation of the tangency line, we need to comptue h

h = a - (2*b*rf) + (c*rf*rf)

return(list(tangentWeights = w.tangent.vector, mupTangent = mup.tangent, sigmaTangent = sigma.tangent, h = h))
}
