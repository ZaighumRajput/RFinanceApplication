##Basic European Call/Put Option Pricer
##Using the Black-Scholes Framework
## Source: http://sas.uwaterloo.ca/~dlmcleis/s906/chapt8.pdf

## 1 means call, 2 means put
##Replace ... = 1 in function to suit your own needs
BSPricer <- function(CallOrPut = 1, SpotPrice = 20, Strike = 19, Vol = 0.25, Maturity = 1, RiskFree = 0.06)
{
  d1 = (log(SpotPrice/Strike) + (RiskFree + Vol^2/2)*T)/Vol*sqrt(T)
  d2 = d1 - Vol*sqrt(T)
  
  if(CallOrPut){
  Price = Strike*exp(-RiskFree*Maturity)*pnorm(-d1) - SpotPrice*pnorm(-d2)
  
  Delta = pnorm(d1) - 1
  
  ##should replace with explicit formula
  Gamma = (exp(-d1^2 /2))/(SpotPrice*sqrt(2*pi)*Vol)
  return(list(Price = Price, Delta = Delta, Gamma = Gamma))}
  if(!CallOrPut){
  Price = pnorm(d1)*SpotPrice - pnorm(d2)*Strike*exp(-RiskFree*T)
  Delta = pnorm(d1)
  
  Gamma = (exp(-d1^2 /2))/(SpotPrice*sqrt(2*pi)*Vol) ##Gamma of Call and Option are equal
  
  
  return(list(Price = Price, Delta = Delta, Gamma = Gamma)) 
  }
}


