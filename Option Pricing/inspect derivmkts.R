rm(list=ls())
#install.packages("derivmkts")
require("derivmkts")
view(arithasianmc)

s <- 100 #price of asset 
k <- 90 #strike
v <- 0.2 #volatility
r <-  0.05 #continuously compounded risk-free rate
tt <-  1/12 #time to maturity n years
d <- 0.05#div yield
m <- 20  #Number of prices in avg calc
numsim <- 10  #Number of monte Carlo iterations

set.seed(420)
"Z - creates a matrix of draws from a normal distribution (Simulate Brownian Motion for Monte Carlo Simulation)"
"Matrix Dimensions (Rows: Simulation runs, Cols: Price Mark to Market Update)"
"Price dimension is set to 20 (IDEA: Mark to Market over 20 trading days - 1 month)"
z <- matrix(rnorm(m * numsim), numsim, m)
"Update normal distribution matrix by cumulatively adding row wise (To simulate price movements of stock)"
zcum <- t(apply(z, 1, cumsum))
"Scale the time interval to the number of prices to be calculated"
h <- tt/m
"Create a 1's matrix that will be updated with prices based on zcum matrix"
S <- matrix(1, nrow = numsim, ncol = m)

"Loop through S Matrix, updating Asset price based on corresponding zcum value(Brownian Motion)"
for (i in 1:m) {
  S[, i] <- s * exp((r - d - 0.5 * v^2) * h * i + 
                      v * sqrt(h) * zcum[, i])
}

"Ending value of each Simulation run"
ST <- S[, m]

"Average Price of each simulation (Required for evaluation of Asian Option"
Savg <- switch("arith", arith = apply(S, 1, sum)/m, geom = apply(S, 1, prod)^(1/m))
Savg

Option_Prices <- pmax(Savg - k, 0)
avgpricecall <- mean(Option_Prices) * exp(-r * tt)
avgpricecallsd <- sd(Option_Prices) * exp(-r * tt)


tmp <- pmax(ST - Savg, 0)
avgstrikecall <- mean(tmp) * exp(-r * tt)
avgstrikecallsd <- sd(tmp) * exp(-r * tt)

tmp <- pmax(ST - k, 0)
bscall <- mean(tmp) * exp(-r * tt)
bscallsd <- sd(tmp) * exp(-r * tt)
