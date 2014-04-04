library(rstan)

# 
signals = seq(0.5,5,by=0.5)
n_noise = c(25,50,100,200,500,10000,2000,5000,10000)

set.seed(1)
y = rnorm(max(n_noise)+length(signals), 
          c(signals,rep(0,max(n_noise))))

hist(y)

m = stan_model("horseshoe.stan")


r = sampling(m, list(n=60, y=y[1:60]), "kappa")
