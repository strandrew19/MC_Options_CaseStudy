#Background Information:

#Methods for Calculating Options (American and European):

#Least Square Monte Carlo
## Valuing Early Exercise Options (Bermudan or American)
## 1. Backward Induction: Assign value at every state at every timestep
## 2. Value is LS regression against market price of option value at that state and time
## 3. When all states are valued, optimal decision on option exercise is made

#Lattice Based Computation (Binomial Options Pricing Model)
##Closed Form solution of Black-Scholes 
##1. Create a tree structure where trees represent price levels
##2. Calculate option value at each final node 
##3. Find Option values at earlier nodes
##Note: Follows a binomial distribution (approaches log normal dist that is assumed by BS)

#Finite Difference methods
##PDE method to approximate continuous-time differential equations
##1. Run from time 0 to maturity
##2. Calculate maturity values (difference between exercise and underlying values)
##3. Calculate values at boundaries (zero and an some large value)
##4. Recursively calculate a lattice structure (many points at each node)

#Asian Options and Look-back options benefit most from MC
## Given Path dependent structure, there is no closed form solution
## Three previous methods grow exponential in complexity given # of paths to calculate
## Monte Carlo simulation is the best choice here.
## While MC is inefficient under closed form problems it shines under open
## MC provides better approximations of actual options price

#From Wikipedia:
#M. Broadie and P. Glasserman were the first to price AO by MC

#Clear Global Enviroment
rm(list=ls())

#Variables to be used:

#Initial price of asset : S_0 
#strike : k
#volatility/standard deviation: vol
#continuously compounded risk-free rate: r
#Time to maturity n years: T_years (252 trading days in year)
#Number of prices in avg calc : m
#Number of Monte Carlo iterations : numsim 
#div yield : d  (This one will be ignored for now)


#https://berkorbay.github.io/fe522/02_Monte_Carlo_Simulation.html#antithetic-variates
#Let's build a function to simulate
ECO<-function(S_0=100,K=100,vol=0.25,T_years=1,r=0.02,numsim=10^4){
  #Simulate the stock
  sim_S_T<-S_0*exp((r-0.5*vol^2)*T_years + vol*rnorm(numsim)*sqrt(T))
  #Calculate payoffs
  payoffs<-pmax(sim_S_T-K,0)*exp(-r*T_years)
  #Simulate results and bounds
  Price<-mean(payoffs)
  SE<-1.96*sd(payoffs)/sqrt(numsim)
  LowerB <- Price - SE
  UpperB <- Price + SE
  return(c(Price=Price,SE=SE,Lower=LowerB,Upper=UpperB))
}
#Simulation with 1000 instances
ECO(S_0=100,K=100,vol=0.25,T_years=1,r=0.02,numsim=10^3)
#Simulation with 10000 instances
ECO(S_0=100,K=100,vol=0.25,T_years=1,r=0.02,numsim=10^4)
#Simulation with 100000 instances
ECO(S_0=100,K=100,vol=0.25,T_years=1,r=0.02,numsim=10^5)

#As we see, the increased number of simulations reduce the variance of the estimate

#To evaluate the true value of the option in this closed case we look at the black-scholes formula

#Real Value
Black_Scholes_call <- function(S_0=100,K=100,r=0.02,T_years=1, vol=0.25){
  d1_ = (log(S_0/K)+(r+vol^2/2)*T_years)/(vol*sqrt(T_years))
  d2_= (log(S_0/K)+(r-vol^2/2)*T_years)/(vol*sqrt(T_years))
  C_bseu= S_0*pnorm(d1_)-K*exp(-r*T_years)*pnorm(d2_) 
  return(C_bseu)
}
Black_Scholes_call(S_0=100,K=100,r=0.02,T_years=1, vol=0.25)

#Visual demonstration of CI shrinking with # of simulations increasing
set.seed(420)

bs_simul <- data.frame(instances = 50,
                       t(sim_european_call(S_0=100,K=100,vol=0.25,T_years=1,r=0.02,numsim=50)))

for(i in 2:1000){
  set.seed(420)
  bs_simul <- rbind(bs_simul,
                    data.frame(instances=50*i,
                               t(sim_european_call(S_0=100,K=100,vol=0.25,T_years=1,r=0.02,numsim=50*i))))
}

bs_price <- Black_Scholes_call(S_0=100,K=100,r=0.02,T_in_days=1, vol=0.25)

require(ggplot2)
#Plot the progress of the bounds and price estimate
ggplot(data=bs_simul) + geom_line(aes(x=instances,y=Price)) +
  geom_line(aes(x=instances,y=Lower),color="blue",linetype=2) +
  geom_line(aes(x=instances,y=Upper),color="blue",linetype=2) +
  geom_hline(yintercept=bs_price,color="red") +
  ylim(c(min(bs_simul$Lower),max(bs_simul$Upper))) + theme_bw()


"Moving on to Asian Options"

#Problem: How do you know the final price and the paths to get there?
#Given Asian Options are path dependent, we should employ MC

#Clean up Vars
rm(list=ls())

"Again we display MC for ECO"

#Generate prices for European Call Option using MC
ECO<-function(S_0=100,K=100,vol=0.2,T_years=1,r=0.02,numsim=10^4){
  #Simulate the stock
  sim_S_T<-S_0*exp((r-0.5*vol^2)*T_years + vol*rnorm(numsim)*sqrt(T))
  #Calculate payoffs
  payoffs<-pmax(sim_S_T-K,0)*exp(-r*T_years)
  #Simulate results and bounds
  Price<-mean(payoffs)
  SE<-1.96*sd(payoffs)/sqrt(numsim)
  LowerB <- Price - SE
  UpperB <- Price + SE
  return(c(Price=Price,SE=SE,Lower=LowerB,Upper=UpperB))
}

"Given Variance is bad, we look at using Antithetical Variates"

#Simulate payoffs with regular and antithetical
ECO_Av <-function(s=100,k=100,v=0.2,T_years=1,r=0.05,numsim=10^4){
  #normal Draws(halved given other half are provided by antithetical)
  z <- matrix(rnorm(m * numsim/2), numsim/2, m)
  #antithetical draw
  anti_z <- -z
  #Simulate payoffs with both processes
  sim_payoff_1<-exp(-r*T_years)*pmax(s*exp((r-0.5*v^2)*T_years + v*z*sqrt(T_years))-k,0)
  sim_payoff_2<-exp(-r*T_years)*pmax(s*exp((r-0.5*v^2)*T_years + v*anti_z*sqrt(T_years))-k,0)
  sim_payoff <- (sim_payoff_1 + sim_payoff_2)/2
  #Calc results and bounds
  Price<-mean(sim_payoff)
  SE<-1.96*sd(sim_payoff)/sqrt(numsim/2)
  LowerB <- Price - SE
  UpperB <- Price + SE
  return(c(Price=Price, SE=SE,Lower=LowerB, Upper=UpperB))
}

#Compare Variance of ECO with AV vs without
ECO()
ECO_Av()


"On to Control Variates"

ECO_CV<-function(s=100,k=100,v=0.2,T_years=1,r=0.02,numsim=10^4, with_naive=TRUE){
  #Simulate Final Stock values
  ST <- s*exp((r-0.5*v^2)*T_years + v * rnorm(numsim) * sqrt(T_years))
  #Simulate Payoffs
  sim_payoff<-exp(-r*T_years)*pmax(ST-k,0)
  #Calculate Theta_star
  theta_star<-cov(ST,sim_payoff)/var(ST)
  #Calculate CV effect
  payoff_cv <- sim_payoff - theta_star*(ST - s*exp(r*T_years))
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

#European Call Option with Control Variate
ECO_CV

"MC for Vanilla Asian Option"

ACO <- function(s=100,k=100,v=0.2,T_years=20/252,r=0.05, m = 20, numsim=10^4){
  "Z - creates a matrix of draws from a normal distribution (Simulate Brownian Motion for Monte Carlo Simulation)"
  "Matrix Dimensions (Rows: Simulation runs, Cols: Price Mark to Market Update)"
  "Price dimension is set to 20 (IDEA: Mark to Market over 20 trading days - 1 month)"
  z <- matrix(rnorm(m *numsim), numsim, m)
  "Update normal distribution matrix by cumulatively adding row wise (To simulate price movements of stock)"
  zcum <- t(apply(z,1,cumsum))
  "Scale the time interval to the number of prices to be calculated"
  h <- T_years/m
  "Create a 1's matrix that will be updated with prices based on zcum matrix"
  S <- matrix(1, nrow = numsim, ncol = m)
  "Loop through S Matrix, updating Asset price based on corresponding zcum value(Brownian Motion)"
  for (i in 1:m){
    S[, i] <- s * exp((r - 0.5 * v^2) * h * i + 
                        v * sqrt(h) * zcum[, i])
  }
  "Ending value of each Simulation run"
  ST <- S[,m]
  
  #Evaluations with Black Scholes
  tmp <- pmax(ST - k, 0)
  tmp <- tmp * exp(-r * T_years)
  bscall <- mean(tmp) 
  bscallsd <- 1.96*sd(tmp)/sqrt(numsim)
  bslower <- bscall - bscallsd
  bsupper <- bscall + bscallsd
  "Average Price of each simulation (Required for evaluation of Asian Option"
  Savg <- switch("arith", arith = apply(S, 1, sum)/m, geom = apply(S, 1, prod)^(1/m))
  "Average Price of Asian Call"
  Option_Prices <- pmax(Savg - k, 0)
  "Discount to Present Value"
  Option_Prices <- Option_Prices * exp(-r * T_years)
  avgpricecall <- mean(Option_Prices) 
  avgpricecallsd <- 1.96*sd(Option_Prices)/sqrt(numsim)
  aolower <- avgpricecall - avgpricecallsd 
  aoupper <- avgpricecall + avgpricecallsd
  "Return Matrix of ouputs"
  output <- matrix(c(bscall,avgpricecall,bscallsd,avgpricecallsd,bslower,aolower,bsupper,aoupper), 2, 4)
  colnames(output) <- c("Call", "SE Call", "Lower", "Upper")
  rownames(output) <- c("Vanilla (BS)", "Asian Avg Price")
  return(output)
}

#Vanilla Asian Call Option and its Black-Scholes equivalent(evaluate only final price)
ACO()

#Function from https://berkorbay.github.io/fe522/02_Monte_Carlo_Simulation.html#antithetic-variates for reference
ACO <- function(s=100,k=100,v=0.2,t_i=c(1:20),r=0.05,numsim=10^4){
    #Assume the last element of t_i is maturity
    #t_i<-t_i/252 #Annualize time periods
    #"Start drawing from a normal distribution (Simulate Brownian Motion for Monte Carlo Simulation) for stock movement"
    #sT<-s*exp((r-v^2/2)*t_i[1] + v*sqrt(t_i[1])*rnorm(numsim))
    #"Sum_ST tracks price movement"
    #sum_sT<-sT
    #"Begin following periods until maturity"
    #for(i in 2:length(t_i)){
    #  dt<-t_i[i]-t_i[i-1]
    #  sT<-sT*exp((r-v^2/2)*dt + v*sqrt(dt)*rnorm(numsim))
    #  sum_sT<-sum_sT + sT}
    #price <- pmax(sum_sT/length(t_i) - k,0)*exp(-r*t_i[length(t_i)])
    #mean_price<-mean(price)
    #SE_price <- 1.96*sd(price)/sqrt(numsim)
    #lower_price <- mean_price - SE_price
    #upper_price <- mean_price + SE_price
    #"Return Matrix of ouputs"
    #output <- matrix(c(mean_price,SE_price,lower_price,upper_price), 1, 4)
    #colnames(output) <- c("Call", "SE Call", "Lower", "Upper")
    #rownames(output) <- c("Asian Avg Price")
    #return(output)
    }

ACO_AV <- function(s=100,k=100,v=0.2,tt=20/252,r=0.05, m = 20, numsim=10){
  seed_gen <- sample(1:1000, 1)
  set.seed(seed_gen)
  #Test without control variates for comparison
  test <- matrix(rnorm(m *numsim), numsim, m)
  tzcum <- t(apply(test,1,cumsum))
  h <- tt/m
  S_test <- matrix(1, nrow = numsim, ncol = m)
  for (i in 1:m){
    S_test[, i] <- s * exp((r - 0.5 * v^2) * h * i + 
                        v * sqrt(h) * tzcum[, i])
  }
  ST_test <- S_test[,m]
  Savg_test <- switch("arith", arith = apply(S_test, 1, sum)/m, geom = apply(S_test, 1, prod)^(1/m))
  Option_Prices_test <- pmax(Savg_test - k, 0)
  Option_Prices_test <- Option_Prices_test* exp(-r * tt)
  avgpricecall_test <- mean(Option_Prices_test) 
  avgpricecallsd_test <- 1.96*sd(Option_Prices_test)/sqrt(numsim)
  aolower <- avgpricecall_test - avgpricecallsd_test 
  aoupper <- avgpricecall_test + avgpricecallsd_test
  
  #Antithetical Variate
  set.seed(seed_gen)
  
  z <- matrix(rnorm(m *numsim/2), numsim/2, m)
  anti_z <- -z
  
  zcum <- t(apply(z,1,cumsum))
  antizcum <- t(apply(anti_z,1,cumsum))
  
  h <- tt/m
  
  S <- matrix(1, nrow = numsim/2, ncol = m)
  SA <- matrix(1, nrow = numsim/2, ncol = m)
  
  for (i in 1:m){
    S[, i] <- s * exp((r - 0.5 * v^2) * h * i + v * sqrt(h) * zcum[, i])}
  for (i in 1:m){
    SA[, i] <- s * exp((r - 0.5 * v^2) * h * i + v * sqrt(h) * antizcum[, i])}
  Savg <- switch("arith", arith = apply(S, 1, sum)/m, geom = apply(S, 1, prod)^(1/m))
  SAavg <- switch("arith", arith = apply(SA, 1, sum)/m, geom = apply(SA, 1, prod)^(1/m))
  Option_Prices <- pmax(Savg - k, 0)
  Option_Prices <- Option_Prices * exp(-r * tt)
  A_Option_Prices <- pmax(SAavg - k, 0)
  A_Option_Prices <- Option_Prices * exp(-r * tt)

  price_Av <- (Option_Prices + A_Option_Prices)/2
  
  avgpricecall <- mean(price_Av) 
  avgpricecallsd <- 1.96*sd(price_Av)/sqrt(numsim) 
  avlower <- avgpricecall - avgpricecallsd
  avupper <- avgpricecall + avgpricecallsd
  output <- matrix(c(avgpricecall_test,avgpricecall,avgpricecallsd_test,avgpricecallsd,aolower,avlower,aoupper,avupper), 2, 4)
  colnames(output) <- c("Call", "SE Call", "Lower", "Upper")
  rownames(output) <- c("Vanilla Asian Option", "AV Asian Option")
  return(output)
}

#Code is is just used to show AV do reduce variance for AO
ACO_AV()

"Plot of Stock Movement under Monte Carlo"
Quick_AO <- function(s=100,k=100,v=0.2,t_i=c(1:20),r=0.05,numsim=10^4){
  z <- matrix(rnorm(m *numsim), numsim, m)
  zcum <- t(apply(z,1,cumsum))
  h <- tt/m
  S <- matrix(1, nrow = numsim, ncol = m)
  for (i in 1:m){
    S[, i] <- s * exp((r - 0.5 * v^2) * h * i + v * sqrt(h) * zcum[, i])}
  return(S)
}
df=as.data.frame(Quick_AO()) 
df <- data.frame(append(df, c(100), after=0))
colnames(df) =sprintf("Day %s", c(0:20))
# To Manually Change y axis: yaxt="n"
matplot(t(df), type="l", main = "Monte Carlo Stock Prices with Asian Option (10,000 simulations) ",xlab = "Days", ylab = "Stock Price", xaxs="i", yaxs="r", xaxt="n")
axis(1,at=c(1, 6, 11, 16, 21),labels=c(0, 5, 10, 15, 20))
#axis(2,at=c(80, 90, 100, 110, 120),labels=c(80, 90, 100, 110, 120))
legend(2, 120, legend=c("Volatility = 0.2", "Risk Free Rate = 0.05", "Starting Stock Value = 100" ), fill=topo.colors(3), cex=0.8)

"Plot of Stock Movement with Antithetical Variables"
Quick_AV <- function(s=100,k=100,v=0.2,t_i=c(1:20),r=0.05,numsim=10^4){
  z <- matrix(rnorm(m *numsim/2), numsim/2, m)
  anti_z <- -z
  zcum <- t(apply(z,1,cumsum))
  antizcum <- t(apply(anti_z,1,cumsum))
  h <- tt/m
  S <- matrix(1, nrow = numsim/2, ncol = m)
  SA <- matrix(1, nrow = numsim/2, ncol = m)
  for (i in 1:m){
    S[, i] <- s * exp((r - 0.5 * v^2) * h * i + v * sqrt(h) * zcum[, i])}
  for (i in 1:m){
    SA[, i] <- s * exp((r - 0.5 * v^2) * h * i + v * sqrt(h) * antizcum[, i])}
  S <- rbind(S, SA)
  return(S)
}
df=as.data.frame(Quick_AV()) 
df <- data.frame(append(df, c(100), after=0))
colnames(df) =sprintf("Day %s", c(0:20))
matplot(t(df), type="l", main = "Monte Carlo Stock Prices with Antithetical Variables (10,000 simulations) ",xlab = "Days", ylab = "Stock Price", xaxs="i", yaxs="r", xaxt="n")
axis(1,at=c(1, 6, 11, 16, 21),labels=c(0, 5, 10, 15, 20))
#axis(2,at=c(80, 90, 100, 110, 120),labels=c(80, 90, 100, 110, 120))
legend(2, 120, legend=c("Volatility = 0.2", "Risk Free Rate = 0.05", "Starting Stock Value = 100" ), fill=topo.colors(3), cex=0.8)


