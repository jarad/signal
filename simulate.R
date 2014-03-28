require(reshape2)
require(ggplot2)
library(ars)
library(dplyr)
library(rstan)

## Models
# 
rpoint_mass = function(n,zero_prob=.8) ifelse(rbinom(n,1,zero_prob),0,rnorm(n))
rhorse_shoe = function(n) rnorm(n,0,abs(rt(n,1)))

n = 100
sims = 
  melt(
    data.frame(
      normal=rnorm(n),
      t5=rt(n,5),
      cauchy = rt(n,1),
      point_mass = rpoint_mass(n,.8),
      horse_shoe = rhorse_shoe(n)),
    variable.name="model", value.name="signal") %.%
  mutate(y=rnorm(length(signal), signal)) %.%
  group_by(model)

qplot(x=signal, data=sims, facets=~model)

sims %.% summarise(IQR(signal),max(signal))


# normal
y2 = sum(((sims %.% subset(model=="normal"))$y)^2)
f  = function(x) -n*log(1+x)/2 - y2/(1+x)  /2
fp = function(x) -n   /(1+x)/2 + y2/(1+x)^2/2
curve(f,0,2)

y = sims$y[sims$model=="normal"]
m = stan_model("normal.stan")
r = sampling(m, data=list(n=length(y), y=y), 
             pars=c("tau"), iter=10000, thin=10)
tau = extract(r, "tau")[[1]]

samps = ars(10000,f,fp,x=1:10, lb=TRUE)
samps = sqrt(samps)
hist(samps, 100, prob=TRUE)
curve(dnorm(x, mean(samps), sd(samps)), add=TRUE, col="red")
curve(dnorm(x, y2/n-1, y2/n*sqrt(2/n)), add=TRUE, col="blue")





# t-distribution
m_t = stan_model("t.stan")
r_t = sampling(m_t, data=list(df=5, n=length(y), y=y), 
             pars=c("tau"), iter=10000, thin=10)
tau2 = extract(r_t, "tau")[[1]]

hist(tau2, 100, prob=TRUE)

# as a scale mixture
m_t_scale_mixture = stan_model("t_scale_mixture.stan")
r_t_scale_mixture = sampling(m_t_scale_mixture, 
                             data=list(df=5, n=length(y), y=y), 
                             pars=c("tau"), 
                             iter=10000, thin=10)
tau = extract(r_t_scale_mixture, "tau")[[1]]

hist(tau, 100, prob=TRUE)



# lasso
m_lasso = stan_model("lasso.stan")
r_lasso = sampling(m_lasso, data=list(n=length(y), y=y), 
               pars=c("tau"), iter=10000, thin=10)
tau = extract(r_lasso, "tau")[[1]]

hist(tau, 100, prob=TRUE)


# lasso as a scale mixture
m_lasso_scale_mixture = stan_model("lasso_scale_mixture.stan")
r_lasso_scale_mixture = sampling(m_lasso_scale_mixture, 
                                 data=list(n=length(y), y=y), 
                   pars=c("tau"), iter=10000, thin=10)
tau = extract(r_lasso_scale_mixture, "tau")[[1]]

hist(tau, 100, prob=TRUE)

# horseshoe
m_horseshoe = stan_model("horseshoe.stan")
r_horseshoe = sampling(m_horseshoe, 
                       data=list(n=length(y), y=y), 
                       pars=c("tau"), iter=10000, thin=10)
tau = extract(r_horseshoe, "tau")[[1]]

hist(tau, 100, prob=TRUE)


