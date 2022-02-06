# Option pricing with Monte Carlo simulations
## Monte Carlo Simulation Methods, Trier University, Winter semester 2021/2022

The goals of this research case study are to Evaluate pricing methodology for Options with no closed form solution (American Options & Arthematic Mean Asian Options)


<details><summary>Black Scholes Model (Call Option)</summary>
<p>
  
| Variables | Description |
| --------- | ----------- |
| **C**     |  Call Option Price |
| **N**     | CDF of the Normal Distribution |
| **S(t)**    | Spot Price of Asset |
| **K**     | Strike price |
| **r**     | Risk-free interest rate (Compounded Annually) |
| **t**     | time to maturity |
| **σ**     | volatility of the asset | 

![Black Scholes Call](https://user-images.githubusercontent.com/62930497/151790833-0f3a08e5-5053-4544-b41c-091ce2504cd2.PNG)

>Information above provided by Pricing Asian Options using Monte Carlo Methods -  pg 4
  
</p>
</details>

<details><summary>Pricing a European Call Option</summary>
<p>
  
| Variables | Description |
| --------- | ----------- |
| **S(T)**     |  Call Option Price |
| **N**     | CDF of the Normal Distribution |
| **S(t)**    | Spot Price of Asset |
| **K**     | Strike price |
| **r**     | Risk-free interest rate (Compounded Annually) |
| **T**     | Time at Expiration |
| **t**     | Current Time |
| **σ**     | volatility of the asset | 
| **Q**     | Probability Measure |
| **μ**     | Mean |
| **X**     | X ~ N(0,1) |
  
 Standard Brownian motion:
  
 ![Standard Brownian Motion](https://user-images.githubusercontent.com/62930497/151795420-627f880e-c560-4e38-af2b-913a5c75e1fa.PNG)
  
 European Call Option Pricing 
  
 Equation 1: Explicit Brownian Motion
  
 Equation 2: Draw from Standard Normal Distribution
  
 ![European Call Option](https://user-images.githubusercontent.com/62930497/151795578-ef8cdabe-21b1-46b4-a179-050c39b72157.PNG)

>Information above provided by Pricing Asian Options using Monte Carlo Methods -  pg 4 
  
</p>
</details>

<details><summary>Control Variates </summary>
<p>
  
To estimate: 
  
![Estimate](https://user-images.githubusercontent.com/62930497/151942390-bdf7b444-eb7d-4045-9892-8d1309b0a4c6.PNG)

where 
 
![Function](https://user-images.githubusercontent.com/62930497/151942419-3905bddc-d929-48b6-9b63-8e7fde6fcdc3.PNG)
 
When we find another random Variable Z with known mean E(Z), we can construct unbiased estimators of θ:
  
![Unbiased Estimators](https://user-images.githubusercontent.com/62930497/151942643-2e822d89-94e5-4dff-a44d-3ca2b13453b7.PNG)

where c is a real number, it is clear that:
  
![Control Estimator](https://user-images.githubusercontent.com/62930497/151942795-bd34f83b-f042-4d8e-be3b-704af3a5dee2.PNG)

Thus Monte Carlo can be applied to the new estimator. Z is the control variate and we have to check whether it has lower variance. Given:

![Control Variate Variance](https://user-images.githubusercontent.com/62930497/151942935-0bf208d7-5efe-4325-b137-88eb1958f851.PNG)

We know c can be any real number and we an choose a c such that it minimizes the quadratic such that when:
  
![Quadratic Minimization](https://user-images.githubusercontent.com/62930497/151943105-cd6f8c21-cef8-4b57-9c1a-753b8f250fb0.PNG)
 
Is plugged into the quadratic:
  
![Quadratic](https://user-images.githubusercontent.com/62930497/151943632-bed5c3cc-6d49-4635-8aaa-910f5fec1da0.PNG)

As long as Cov(Y, Z)<img src="https://render.githubusercontent.com/render/math?math=\neq">0, variance reduction is achieved. While Cov(Y, Z) is typically not known, it is possible to estimate it during the Monte Carlo simulation.
  
Often the difficult to find a suitable control variate. In this case two options are avalible:
  
Given the Geormetic Average Asian Call has a closed form solution, it can be used as a control variate for variance reduction
  
![Geometric Asian Call Option](https://user-images.githubusercontent.com/62930497/151939271-a22f1ba6-7dd3-44e4-845f-68ebc456acf7.PNG)

Another option is the European Call Option as a control variate:

![ECO](https://user-images.githubusercontent.com/62930497/151939293-0ac8ce6b-326f-4421-bb54-f73f2bb0c8fc.PNG)

>Information above provided by Pricing Asian Options using Monte Carlo Methods - Hongbin Zhang pg 20-21
  
</p>
</details>

<details><summary>Crude Monte Carlo and Antithetic Variables</summary>
<p>

Another idea regarding variance reduction os is using negatively correlated variates (Antithetic Variables)
 
Given the expectation:
  
![Expectation](https://user-images.githubusercontent.com/62930497/151938099-e7f644e7-15b1-4f9b-a865-d238c65c5473.PNG)

The crude Monte Carlo is given by:
  
![Crude Monte Carlo](https://user-images.githubusercontent.com/62930497/151938112-00c5500c-603a-4bb7-b24b-55ab61d13ce6.PNG)

The estimate by the antithetic variate method is:
  
![Antithetic Variables](https://user-images.githubusercontent.com/62930497/151938129-1b841f7b-9788-42b9-8bec-4b6bcf274739.PNG)

Given the two antithetic variates are negatively correlated, if the function g is monotonic than variance reduction can be achieved

>Information above provided by Pricing Asian Options using Monte Carlo Methods - Hongbin Zhang pg 19-20 and 24
</p>
</details>
  
Resources:
Monte Carlo Methods in Financial Engineering - Paul Glasserman

Pricing Asian Options using Monte Carlo Methods - Hongbin Zhang

https://cran.r-project.org/web/packages/derivmkts/derivmkts.pdf

https://berkorbay.github.io/fe522/02_Monte_Carlo_Simulation.html#antithetic-variates
