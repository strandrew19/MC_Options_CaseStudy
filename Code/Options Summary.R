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

#M. Broadie and P. Glasserman were the first to price AO by MC


#Let's build a function to simulate
sim_european_call<-function(S_0=100,K=100,vol=0.25,T_years=1,r=0.02,n=10^4){
  #Simulate the stock
  sim_S_T<-S_0*exp((r-0.5*vol^2)*T_years + vol*rnorm(n)*sqrt(T))
  #Calculate payoffs
  payoffs<-pmax(sim_S_T-K,0)*exp(-r*T_years)
  #Simulate results and bounds
  Price<-mean(payoffs)
  SE<-1.96*sd(payoffs)/sqrt(n)
  LowerB <- Price - SE
  UpperB <- Price + SE
  return(c(Price=Price,SE=SE,Lower=LowerB,Upper=UpperB))
}
#Simulation with 1000 instances
sim_european_call(S_0=100,K=100,vol=0.25,T_years=1,r=0.02,n=10^3)
#Simulation with 10000 instances
sim_european_call(S_0=100,K=100,vol=0.25,T_years=1,r=0.02,n=10^4)
#Simulation with 100000 instances
sim_european_call(S_0=100,K=100,vol=0.25,T_years=1,r=0.02,n=10^5)

#Real Value
Black_Scholes_call <- function(S_0=100,K=100,r=0.02,T_in_days=252, vol=0.25){
  d1_ = (log(S_0/K)+(r+vol^2/2)*T_in_days)/(vol*sqrt(T_in_days))
  d2_= (log(S_0/K)+(r-vol^2/2)*T_in_days)/(vol*sqrt(T_in_days))
  C_bseu= S_0*pnorm(d1_)-K*exp(-r*T_in_days)*pnorm(d2_) 
  return(C_bseu)
}
Black_Scholes_call(S_0=100,K=100,r=0.02,T_in_days=1, vol=0.25)

#Demonstration of CI shrinking with # of simulations increasing
set.seed(420)

bs_simul <- data.frame(instances = 50,
                       t(sim_european_call(S_0=100,K=100,vol=0.25,T_years=1,r=0.02,n=50)))

for(i in 2:1000){
  set.seed(420)
  bs_simul <- rbind(bs_simul,
                    data.frame(instances=50*i,
                               t(sim_european_call(S_0=100,K=100,vol=0.25,T_years=1,r=0.02,n=50*i))))
}

bs_price <- Black_Scholes_call(S_0=100,K=100,r=0.02,T_in_days=1, vol=0.25)

require(ggplot2)
#Let's plot the progress of the bounds and price estimate
ggplot(data=bs_simul) + geom_line(aes(x=instances,y=Price)) +
  geom_line(aes(x=instances,y=Lower),color="blue",linetype=2) +
  geom_line(aes(x=instances,y=Upper),color="blue",linetype=2) +
  geom_hline(yintercept=bs_price,color="red") +
  ylim(c(min(bs_simul$Lower),max(bs_simul$Upper))) + theme_bw()

