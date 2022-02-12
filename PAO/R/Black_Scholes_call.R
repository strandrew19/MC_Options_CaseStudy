#' Black-Scholes Call Pricing
#'
#' @description This function provides pricing for a Black-Scholes Call Option
#' @param s Starting Stock Price
#' @param k Strike Price
#' @param r Risk Free Rate - Annualized
#' @param tt Days Until Maturity - Annualized
#' @param v Volatility Of Stock - Stock Volatility Return on Annual Basis
#' @return Price of Black-Scholes European Call Option
#' @examples
#' Black_Scholes_call(100, 90, 0.05, 20/252, 0.20)
#' @returns
#' Evaluate price of a 90 strike call option with 20 days to expiration
#' (one month trading) with a risk free rate of 5%, 20% volatility and
#' stock trading at 100 a share
#' @export

Black_Scholes_call <- function(s=100,k=100,v=0.2,tt=252/252, r=0.05){
  d1_ = (log(s/k)+(r+v^2/2)*tt)/(v*sqrt(tt))
  d2_= (log(s/k)+(r-v^2/2)*tt)/(v*sqrt(tt))
  C_bseu= s*pnorm(d1_)-k*exp(-r*tt)*pnorm(d2_)
  return(C_bseu)
}
