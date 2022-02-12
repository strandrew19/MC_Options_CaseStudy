#' Asian Call Option Pricing with Control Variates (European Call Option)
#'
#' @description This function provides Monte Carlo Pricing for a Asian Call Option with European Call Option as a Control Variate
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
#' ACO_CV_ECO(100, 90, 0.05, 20/252, 0.20, 20, 10^4)
#' @returns
#' Evaluate price of a 90 strike call option with 20 days to expiration
#' (one month trading) and 20 price intervals with a risk free rate of
#' 5%, 20% volatility and stock trading at 100 a share via
#' 10000 simulated prices
#' @export

ACO_CV_ECO <- function(s=100,k=100,v=0.2,tt=252/252,r=0.05, m = 20, numsim=10^4){
  z <- matrix(rnorm(m *numsim), numsim, m)
  zcum <- t(apply(z,1,cumsum))
  h <- tt/m
  S <- matrix(1, nrow = numsim, ncol = m)
  for (i in 1:m){S[, i] <- s * exp((r - 0.5 * v^2) * h * i + v * sqrt(h) * zcum[, i])}
  ST <- S[,m]
  Savg <- switch("arith", arith = apply(S, 1, sum)/m, geom = apply(S, 1, prod)^(1/m))
  Option_Prices <- pmax(Savg - k, 0)
  Option_Prices <- Option_Prices* exp(-r * tt)

  Price_ECO <- pmax(ST - k,0)*exp(-r*tt)
  #Calculate Black_Scholes Price so that we can generate Expectation E(Price of European Option)
  Price_BS <- Black_Scholes_call(s=s,k=k, r=r, tt=tt,v=v)

  theta_star <- cov(Option_Prices,Price_ECO)/var(Price_ECO)

  price_cv <- Option_Prices - theta_star*(Price_ECO - Price_BS)

  mean_price<-mean(price_cv)

  CVavgpricecall <- mean(price_cv)
  CVavgpricecallsd <- 1.96*sd(price_cv)/sqrt(numsim)
  CVaolower <- CVavgpricecall - CVavgpricecallsd
  CVaoupper <- CVavgpricecall + CVavgpricecallsd

  output <- matrix(c(CVavgpricecall,CVavgpricecallsd,CVaolower,CVaoupper), 1, 4)
  colnames(output) <- c("Call", "SE Call", "Lower", "Upper")
  rownames(output) <- c("ECO Control Variate")
  return(output)
}
