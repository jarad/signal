require(reshape2)
require(ggplot2)
library(ars)
library(dplyr)

## Models
# 
rpoint_mass = function(n,zero_prob=.8) ifelse(rbinom(n,1,zero_prob),0,rnorm(n))
rhorse_shoe = function(n) rnorm(n,0,abs(rt(n,1)))

n = 45000
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

samps = ars(10000,f,fp,x=1:10, lb=TRUE)
hist(samps, 100, prob=TRUE)
curve(dnorm(x, mean(samps), sd(samps)), add=TRUE, col="red")
curve(dnorm(x, y2/n-1, y2/n*sqrt(2/n)), add=TRUE, col="blue")

# t-distribution
f = function(x, y) { sum(dt(y,0,5,log=TRUE)) }
