#' Asian Call Option Pricing with Antithetical Variates
#'
#' @description This function provides Monte Carlo Pricing for a Asian Call Option with Antithetical Variates
#' Inspiration from: https://berkorbay.github.io/fe522/02_Monte_Carlo_Simulation.html#antithetic-variates
#' and package(derivmkts)
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
#' ACO_AV(100, 90, 0.05, 20/252, 0.20, 20, 10^4)
#' @returns
#' Evaluate price of a 90 strike call option with 20 days to expiration
#' (one month trading) and 20 price intervals with a risk free rate of
#' 5%, 20% volatility and stock trading at 100 a share via
#' 10000 simulated prices
#' @export

ACO_AV <- function(s=100,k=100,v=0.2,tt=252/252,r=0.05, m = 20, numsim=10^4){
  z <- matrix(rnorm(m *numsim/2), numsim/2, m)
  anti_z <- -z

  zcum <- t(apply(z,1,cumsum))
  antizcum <- t(apply(anti_z,1,cumsum))

  h <- tt/m

  S <- matrix(1, nrow = numsim/2, ncol = m)
  SA <- matrix(1, nrow = numsim/2, ncol = m)

  for (i in 1:m){S[, i] <- s * exp((r - 0.5 * v^2) * h * i + v * sqrt(h) * zcum[, i])}
  for (i in 1:m){SA[, i] <- s * exp((r - 0.5 * v^2) * h * i + v * sqrt(h) * antizcum[, i])}

  Savg <- switch("arith", arith = apply(S, 1, sum)/m, geom = apply(S, 1, prod)^(1/m))
  SAavg <- switch("arith", arith = apply(SA, 1, sum)/m, geom = apply(SA, 1, prod)^(1/m))
  Option_Prices <- pmax(Savg - k, 0)
  Option_Prices <- Option_Prices * exp(-r * tt)
  A_Option_Prices <- pmax(SAavg - k, 0)
  A_Option_Prices <- A_Option_Prices * exp(-r * tt)

  price_Av <- (Option_Prices + A_Option_Prices)/2

  avgpricecall <- mean(price_Av)
  avgpricecallsd <- 1.96*sd(price_Av)/sqrt(numsim)
  avlower <- avgpricecall - avgpricecallsd
  avupper <- avgpricecall + avgpricecallsd
  output <- matrix(c(avgpricecall,avgpricecallsd,avlower,avupper), 1, 4)
  colnames(output) <- c("Call", "SE Call", "Lower", "Upper")
  rownames(output) <- c("AV Asian Option")
  return(output)
}

