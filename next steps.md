# Where we last left off:
## 1. Read more literature 
## 2. Start up on Coding 
## 3. Investigate Monte Carlo Estimation and Variance Reduciton
## 4. Decide on what to do for poster
#
&nbsp;

# Formulas
## Pricing via Black Scholes Model:
#
&nbsp;

$C= N(d_1)S_t - N(d_2) Ke^{-rt}$

where $d_1= ln (\cfrac{S_t}{K}) + \cfrac{(r + \cfrac{\sigma^2}{2})*T}{\sigma \sqrt{T}}$

and $d_2 = ln (\cfrac{S_t}{K}) + \cfrac{(r - \cfrac{\sigma^2}{2})*T}{\sigma \sqrt{T}}$

C = call option price<br>
N = CDF of the normal distribution<br>
St= spot price of an asset<br>
K = strike price<br>
r = risk-free interest rate<br>
T = time to maturity<br>
σ = volatility of the asset

#
## European (Call) Options Priced via Monte Carlo:
### IDEA: Simulate potential paths for a stock price via Brownian motion
#
### Brownian Motion is defined as
$W^Q(t) = W^P(t) +(\cfrac{(\mu - r)}{\sigma})*t$

#### Brownian Motion is defined by a probability space and the motion is simulated via draws (In our case via a normal Distribution) 
&nbsp;
### We can reformulate the option price via the Brownian Motion:
$S(T) = S(t) e^{(r-\cfrac{\sigma^2}{2})*(T-t)+\sigma *(W^Q(T)-W^Q(t))}$

### Reformulated in terms of a Normal Distribution (Where X is Standard Normal Distributed i.e. X ~ N(0,1))
$S(T) = S(t) e^{(r-\cfrac{\sigma^2}{2})*(T-t)+\sigma*(\sqrt{T-t}*X))}$

### To Apply the pricing to Asian options, we simply need to keep track of the paths and average the result
#
# Variance Reduction
## Control Variates
#
## Idea:

To estimate: $\theta: = E(Y)$ where $Y = g(X)$

In other words $g(X)$ is a function that generates X's such that the expectation of $Y$ meets the true value $\theta$

&nbsp;

When we find another random Variable Z with known mean E(Z), we can construct unbiased estimators of θ:
1. $\hat{\theta} = Y$, which is our usual estimator
2. $\hat{\theta}_c = Y + c(Z - E(Z))$ 

where c is a real number, it is clear that: $E(\hat{\theta}_c) = E(Y) + c(E(Z) - E(Z)) = E(Y) = \theta$

Thus Monte Carlo can be applied to the new estimator. Z is the control variate and we have to check whether it has lower variance. Given: $Var(\hat{\theta}_{c_{min}}) = Var(Y) + c_{min}^2 Var(Z) + 2c_{min}Cov(Y,Z)$

We know c can be any real number and we an choose a c such that it minimizes the quadratic such that when: $c_{\text{min}} = -\frac{Cov(Y,Z)}{Var(Z)}$

Is plugged into the quadratic:

$Var(\hat{\theta}_c) = Var(Y) + c^2 Var(Z) + 2c Cov(Y,Z)$

$Var(\hat{\theta}_c)= Var(Y)-\frac{Cov(Y,Z)^2}{Var(Z)}$

$Var(\hat{\theta}_c)= Var(\hat{\theta}) - \frac{Cov(Y,Z)^2}{Var(Z)}$

As long as $Cov(Y, Z) \neq 0$, variance reduction is achieved. While Cov(Y, Z) is typically not known, it is possible to estimate it during the Monte Carlo simulation.
#
## Selecting Covariates:
#

Often the difficult to find a suitable control variate. In this case two options are avalible:

Given the Geormetic Average Asian Call has a closed form solution, it can be used as a control variate for variance reduction

$V = e^{-rt}(e^{\frac{1}{m+1}\sum_{i=0}^m \text{log} S(\frac{iT}{m})} - K)^{+}$

Another option is the European Call Option as a control variate:

$U = e^{-rt}(S(T)-K)^+$

#
## Antithetic Variables
#

Another idea regarding variance reduction os is using negatively correlated variates (Antithetic Variables)

Given the expectation:

$\theta = E(Y) = E(g(X))$, with $i.i.d.$ $X_i$ ~ $N(0,1)$

The crude Monte Carlo is given by:

$\hat{\theta} = \frac{1}{n} \sum_{i=1}^n g(X_i),$ with $i.i.d.$ $X_i$ ~ $N(0,1)$

The estimate by the antithetic variate method is:

$\hat{\theta}_a = \frac{1}{n} \sum_{i=1}^n \frac{g(X_i) + g(-X_i)}{2}$, with $i.i.d.$ $X_i$ ~ $N(0,1)$

Given the two antithetic variates are negatively correlated, if the function g is monotonic than variance reduction can be achieved

#
# From Pricing Asian Options using Monte Carlo Methods (pg 28)

## Our results suggest applying the geometric average Asian option as the control variate to the Monte Carlo approach, since it greatly improves the standard deviation result to provide a narrower confidence interval. We also checked the antithetic variate method, and it turned out that this variance reduction technique was less attractive than the geometric Asian option variate one in our case. 
&nbsp;
## SD w/ European option for cv reduced aprox 50% (very modest gain in efficiency). Example: 95 percent confidence limits for 20-timestep Asian option with a current stock price of 90 have been reduced from + 0.4066 to + 0.0104. To achieve the same reduction by increasing the number of trials would require about 15,306,000 trials instead of 10,000. The confidence limits by using the geometric average Asian option as the control variate would appear to be sufficiently accurate for most practical applications. 

#
# Proposal for Project Next  Steps
## Question: How well can each of the models stand up to increasing volatility?
### Test preformance of 4 pricing models 
1. Asian Options
2. Asian Options with European Option Control Variate 
3. Asian Options with Geometric Asian Options Control Variate
4. Asian Options with Anthithetic Variable
### Outline of Program
1. Choose a 20 day interval with 10,000 simulations (20 day would simulate one month of Trading) 
2. Choose one Intial Stock Price (100 euro) and three call strikes (We will ignore puts) 
    1. Out the money: 110 euro
    2. At the money: 100 euro
    3. In the money: 90 euro
3. Test 6 levels of volatility (High volatility typically destroys Black-Scholes Pricing Model)
    1. $\sigma$ = 0.05
    2. $\sigma$ = 0.20
    3. $\sigma$ = 0.50
    4. $\sigma$ = 1.0
    5. $\sigma$ = 10.00
    6. $\sigma$ = 50.00 (This should break all our models theoretically)
4. Plot results of the 4 pricing models and their Confidence Intervals under the 6 volatility models

Note: Volatility is represented as the annualized standard deviation of continously compounded return













