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
| **r**     | risk-free interest rate |
| **t**     | time to maturity |
| **σ**     | volatility of the asset | 

![Black Scholes Call](https://user-images.githubusercontent.com/62930497/151790833-0f3a08e5-5053-4544-b41c-091ce2504cd2.PNG)

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
| **r**     | risk-free interest rate |
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

</p>
</details>




Resources:
Monte Carlo Methods in Financial Engineering - Paul Glasserman
Pricing Asian Options using Monte Carlo Methods - Hongbin Zhang

https://cran.r-project.org/web/packages/derivmkts/derivmkts.pdf
