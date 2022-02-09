#' Closed Formed Solution of Geometric Average Asian Option
#'
#' @description This function provides a Closed Form Solution to the Geometric Asian Option
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
#' @return Estimated Price of Asian Call Option, Standard Error and One Standard Deviation Confidence Interval 
#' @examples 
#' GEO_AO(100, 90, 0.05, 20/252, 0.20, 20, FALSE)
#' Method Generates volatility and interest rates that are plugged back into the Black-Scholes Formula to produce a Geometric Average Price Estimate

GEO_AO <-function(s=100,k=100,v=0.2,tt=252/252,r=0.05, m = 20){
  #Geometric Asian Black-Scholes Function
  sigma_z <- v * sqrt(((2*m)+1)/(6*(m+1)))
  rho <- ((r-v^2/2)+sigma_z)/2
  GEOM_value <- Black_Scholes_call(s=s,k=k,r=rho,tt=tt, v=sigma_z)
  return(GEOM_value)
}

