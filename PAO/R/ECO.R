#' European Call Option Pricing
#'
#' @description This function provides a crude Monte Carlo Pricing for a European Call Option
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
#' @return Estimated Price of European Call Option, Standard Error and One Standard Deviation Confidence Interval 
#' @examples 
#' ECO(100, 90, 0.05, 20/252, 0.20, 20, 10^4)
#' Evaluate price of a 90 strike call option with 20 days to expiration 
#' (one month trading) and 20 price intervals with a risk free rate of 
#' 5%, 20% volatility and stock trading at 100 a share via 
#' 10000 simulated prices  

ECO<-function(s=100,k=100,v=0.2,tt=252/252,r=0.05, m = 20, numsim=10^4){
  #Normal Draws
  z <- matrix(rnorm(m * numsim), numsim, m)
  #Calculate payoffs
  sim_payoff <-exp(-r*tt)*pmax(s*exp((r-0.5*v^2)*tt + v*z*sqrt(tt))-k,0)
  #Simulate results and bounds
  Price<-mean(sim_payoff)
  SE<-1.96*sd(sim_payoff)/sqrt(numsim)
  LowerB <- Price - SE
  UpperB <- Price + SE
  return(c(Price=Price,SE=SE,Lower=LowerB,Upper=UpperB))
}