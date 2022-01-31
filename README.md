# Option pricing with Monte Carlo simulations
## Monte Carlo Simulation Methods, Trier University, Winter semester 2021/2022

The goals of this research case study are to Evaluate pricing methodology for Options with no closed form solution (American Options & Arthematic Mean Asian Options)


Black Scholes Model<br><br>

<img src="https://render.githubusercontent.com/render/math?math=C= N(d_1)S_t - N(d_2) Ke^{-rt}">

 
<img src="https://render.githubusercontent.com/render/math?math=d_1= \cfrac{ln \cfrac{S_t}{K} + (r + \cfrac{\sigma^2}{2})t}{\sigma \sqrt{t}}">


<img src="https://render.githubusercontent.com/render/math?math=d_2 = d_1 - \sigma \sqrt{t}">

C = call option price<
N = CDF of the normal distribution<br>
St= spot price of an asset<br>
K = strike price<br>
r = risk-free interest rate<br>
t = time to maturity<br>
σ = volatility of the asset

Resources:
Monte Carlo Methods in Financial Engineering - Paul Glasserman
Pricing Asian Options using Monte Carlo Methods - Hongbin Zhang
