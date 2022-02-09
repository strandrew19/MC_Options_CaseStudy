#' European Call Option Pricing with Control Variate (Final Stock Price)
#'
#' @description This function provides Monte Carlo Pricing for a European Call Option with the Final Stock Price as a Control Variate
#' Inspiration from: https://berkorbay.github.io/fe522/02_Monte_Carlo_Simulation.html#antithetic-variates
#' and package(derivmkts)
#' 
#' @param s Starting Stock Price 
#' @param k Strike Price
#' @param v Volatility Of Stock - Stock Volatility Return on Annual Basis
#' @param tt Days Until Maturity - Annualized
#' @param r Risk Free Rate - Annualized
#' @param numsim Number of Price Simulations Run
#' @param with_naive If True, return crude Monte Carlo Estimate for Comparision
#' @return Estimated Price of European Call Option, Standard Error and One Standard Deviation Confidence Interval 
#' @examples
#' ECO_CV(100, 90, 0.05, 20/252, 0.20, 20, 10^4, FALSE)
#' Evaluate price of a 90 strike call option with 20 days to expiration 
#' (one month trading) and 20 price intervals with a risk free rate of 
#' 5%, 20% volatility and stock trading at 100 a share via 
#' 10000 simulated prices and no print out of Crude Monte Carlo Simulation

ECO_CV<-function(s=100,k=100,v=0.2,tt=252/252,r=0.05, numsim=10^4, with_naive=TRUE){
  #Normal Draws
  ST <- s*exp((r-0.5*v^2)*tt + v * rnorm(numsim) * sqrt(tt))
  #Calculate payoffs
  sim_payoff<-exp(-r*tt)*pmax(ST-k,0)
  #Calculate Theta_star
  theta_star<-cov(ST,sim_payoff)/var(ST)
  #Calculate CV effect
  payoff_cv <- sim_payoff - theta_star*(ST - s*exp(r*tt))
  #Calculate the output of naive simulation as well
  if(with_naive){
    Price<-mean(sim_payoff)
    SE<-1.96*sd(sim_payoff)/sqrt(numsim)
    LowerB <- Price - SE
    UpperB <- Price + SE
    print(c(Price_naive=Price,SE_naive=SE,Lower=LowerB,Upper=UpperB))
  }
  #Calculate expected price
  Price<-mean(payoff_cv)
  SE<-1.96*sd(payoff_cv)/sqrt(numsim)
  LowerB <- Price - SE
  UpperB <- Price + SE
  return(c(Price_CV=Price,SE_CV=SE,Lower=LowerB,Upper=UpperB))
}