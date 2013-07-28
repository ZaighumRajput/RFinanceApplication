##VaR simulation

require(e1071)

measures <- function(N=10000, alpha = 0.95, mu = 0, sig = 0.35, n =14, 
                     s_0 = 100, K = 100, T = 1, r = 0.02) {
  VaR = 0
  CTE = 0
  
  #Calculate the portfolio (call) value at time 0
  d1_0 = (log(s_0/K) + (r + 1/2 * sig^2) * T)/sig/sqrt(T)
  d2_0 = d1_0 - sig * sqrt(T)
  c_0 = s_0 * pnorm(d1_0) - K * exp(-r*T) * pnorm(d2_0)
  v_0 = c_0
  
  #Simulate the stock price after n days
  s_n = s_0 * exp(rnorm(N, (mu - 1/2 * sig^2) * n/365, sig * sqrt(n/365)))
  
  #Calculate the portfolio (Call) after n days
  d1_n = (log(s_n/K) + (r + 1/2 * sig^2) * (T - n/365))/sig/sqrt(T - n/365)
  d2_n = d1_n - sig* sqrt(T - n/365)
  c_n = s_n * pnorm(d1_n) - K * exp(-r * (T - n/365)) * pnorm(d2_n)
  v_n = c_n
  
  ##Loss 
  losses = sort(v_0 - v_n)
  ##Replace this with quantile
  pos = alpha * (N + 1)
  k = floor(pos)
  VaR = (pos - k) * losses[k + 1] + (k + 1 - pos) * losses[k]
  
  CTE = mean(losses[ceiling(pos):length(losses)])
  
  return (list(alpha, pos, k))
  
}