##Basic European Call/Put Option Pricer

BSPricer <- function(SpotPrice, Strike, Vol, Maturity, RiskFree)
{
  d1 = (ln(SpotPrice/Strike) + (r + vol^2/2)*T)/Vol*sqrt(T)
  d2 = d1 - Vol*sqrt(T)
}


