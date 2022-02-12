#' Asian Call Option Pricing
#'
#' @description This function provides Monte Carlo Pricing for a Asian Call Option
#' Inspiration from: https://berkorbay.github.io/fe522/02_Monte_Carlo_Simulation.html#antithetic-variates
#' and package(derivmkts)
#'
#'
#' @param s Starting Stock Price
#' @param k Strike Price
#' @param v Volatility Of Stock - Stock Volatility Return on Annual Basis
#' @param tt Days Until Maturity - Annualized
#' @param r Risk Free Rate - Annualized
#' @param m Number of Price Intervals to be Calculated
#' @param numsim Number of Price Simulations Run
#' @return Estimated Price of Asian Call Option, Standard Error and One Standard Deviation Confidence Interval
#' @examples
#' ACO(100, 90, 0.05, 20/252, 0.20, 20, 10^4)
#' @returns
#' Evaluate price of a 90 strike call option with 20 days to expiration
#' (one month trading) and 20 price intervals with a risk free rate of
#' 5%, 20% volatility and stock trading at 100 a share via
#' 10000 simulated prices and Black Scholes Pricing for Comparison
#' @export


ACO <- function(s=100,k=100,v=0.2,tt=252/252,r=0.05, m = 20, numsim=10^4){
  #Z - creates a matrix of draws from a normal distribution (Simulate Brownian Motion for Monte Carlo Simulation)"
  #Matrix Dimensions (Rows: Simulation runs, Cols: Price Mark to Market Update)"
  z <- matrix(rnorm(m *numsim), numsim, m)
  #Update normal distribution matrix by cumulatively adding row wise (To simulate price movements of stock)
  zcum <- t(apply(z,1,cumsum))
  #Scale the time interval to the number of prices to be calculated
  h <- tt/m
  #Create a 1's matrix that will be updated with prices based on zcum matrix
  S <- matrix(1, nrow = numsim, ncol = m)
  #Loop through S Matrix, updating Asset price based on corresponding zcum value(Brownian Motion)
  for (i in 1:m){S[, i] <- s * exp((r - 0.5 * v^2) * h * i + v * sqrt(h) * zcum[, i])}
  #Ending value of each Simulation run"
  ST <- S[,m]

  #Average Price of each simulation (Required for evaluation of Asian Option
  Savg <- switch("arith", arith = apply(S, 1, sum)/m, geom = apply(S, 1, prod)^(1/m))
  #Average Price of Asian Call
  Option_Prices <- pmax(Savg - k, 0)
  #Discount to Present Value
  Option_Prices <- Option_Prices * exp(-r * tt)
  avgpricecall <- mean(Option_Prices)
  avgpricecallsd <- 1.96*sd(Option_Prices)/sqrt(numsim)
  aolower <- avgpricecall - avgpricecallsd
  aoupper <- avgpricecall + avgpricecallsd
  #Return Matrix of ouputs
  output <- matrix(c(avgpricecall,avgpricecallsd,aolower,aoupper), 1, 4)
  colnames(output) <- c("Call", "SE Call", "Lower", "Upper")
  rownames(output) <- c("Asian Avg Price")
  return(output)
}
