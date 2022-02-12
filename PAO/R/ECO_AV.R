#' European Call Option Pricing with Antithetical Variates
#'
#' @description This function provides a Monte Carlo Pricing for a European Call Option with Antithetical Variates
#' Inspiration from: https://berkorbay.github.io/fe522/02_Monte_Carlo_Simulation.html#antithetic-variates
#' and package(derivmkts)
#'
#' @param s Starting Stock Price
#' @param k Strike Price
#' @param v Volatility Of Stock - Stock Volatility Return on Annual Basis
#' @param tt Days Until Maturity - Annualized
#' @param r Risk Free Rate - Annualized
#' @param numsim Number of Price Simulations Run
#' @param m Number of Price Intervals to be Calculated
#' @return Estimated Price of European Call Option, Standard Error and One Standard Deviation Confidence Interval
#' @examples
#' ECO_Av(100, 90, 0.05, 20/252, 0.20, 20, 10^4)
#' @returns
#' Evaluate price of a 90 strike call option with 20 days to expiration
#' (one month trading) and 20 price intervals with a risk free rate of
#' 5%, 20% volatility and stock trading at 100 a share via
#' 10000 simulated prices
#' @export

ECO_Av <-function(s=100,k=100,v=0.2,tt=252/252,r=0.05, m = 20, numsim=10^4){
  #normal Draws(halved given other half are provided by antithetical)
  z <- matrix(rnorm(m * numsim/2), numsim/2, m)
  #antithetical draw
  anti_z <- -z
  #Simulate payoffs with both processes
  sim_payoff_1<-exp(-r*tt)*pmax(s*exp((r-0.5*v^2)*tt + v*z*sqrt(tt))-k,0)
  sim_payoff_2<-exp(-r*tt)*pmax(s*exp((r-0.5*v^2)*tt + v*anti_z*sqrt(tt))-k,0)
  sim_payoff <- (sim_payoff_1 + sim_payoff_2)/2
  #Calc results and bounds
  Price<-mean(sim_payoff)
  SE<-1.96*sd(sim_payoff)/sqrt(numsim/2)
  LowerB <- Price - SE
  UpperB <- Price + SE
  return(c(Price=Price, SE=SE,Lower=LowerB, Upper=UpperB))
}
