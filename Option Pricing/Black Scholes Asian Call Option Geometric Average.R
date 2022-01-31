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
Dt <- 0.1
m = T/Dt

sigsqT <- sigma^2*T*(2*m+1)/(6*m+6)
muT <- 0.5*sigsqT + 0.5*(r-0.5*sigma^2)*T
d1 <- (log(S0/K)+(muT + 0.5*sigsqT))/(sqrt(sigsqT))
d2 <- d1 - sqrt(sigsqT)
geo <- exp(-r*T)*(S0 * exp(muT)*pnorm(d1)-K*pnorm(d2))


ones <- function(x){c(rep(c(1), x))}
ones_matrix <- function(x, y){replicate(y, x)}
norm_matrix <- function(x, y){replicate(y, rnorm(x))}
Path_matrix <- function(x, y){cbind(matrix(ones(n), ncol=1), matrix(y, nrow=n))}

matrix(ones(n), ncol=1)
ranvec <- norm_matrix(n, m)
Spath <- S0 * Path_matrix(n, cumprod(exp(r-0.5*sigma^2)*Dt+sigma*sqrt(Dt)*ranvec))
ESpath <- S0 * Path_matrix(n, cumprod(exp(r*Dt)*ones_matrix(n,m)))
