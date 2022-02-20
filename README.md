# Plackett-Luce Bayesian Model in Stan

This is the code to replicate the data example I used in a presentation at the IOC's conference for Injury and Prevention in November 2021. The talk is about using information well to model performance in sports with ranked data. You can see the slides at this [link](https://statsbystokes.wordpress.com/2022/02/15/careful-what-you-throw-out-modelling-rank-data/).

The stan model should be reasonably easy to adapt to other ranked sports, mostly just need to supply the data in the correct format. I also included code to sample from the posterior predictive.
