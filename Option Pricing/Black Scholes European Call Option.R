rm(list = ls())
#S0 - Spot Price of asset
#K - Strike
#T - Time interval
#t - Time a start
#r - risk-free interest rate
#sigma - volatility of asset
#n - number of simulated paths 

set.seed(420)
S0 <- 100
K <- 90
T <- 1
t <- 0
r <- 0.05
sigma <- 0.2
n <- 10000

d1 <- (log(S0/K)+(r+sigma^2/2)*(T-t))/(sigma*sqrt(T-t)) 
d2 <- (log(S0/K)+(r-sigma^2/2)*(T-t)/(sigma*sqrt(T-t)))
#Black-Scholes European Call Option
C_bseu <- S0*pnorm(d1)-K*exp(-r*(T-t))*pnorm(d2) 
#Black-Scholes European Put Option
P_bseu <- K*exp(-r*(T-t))-S0+C_bseu

