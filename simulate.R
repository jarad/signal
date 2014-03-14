require(reshape2)
require(ggplot2)

## Models
# 
rpoint_mass = function(n,zero_prob=.8) ifelse(rbinom(n,1,zero_prob),0,rnorm(n))
rhorse_shoe = function(n) rnorm(n,0,abs(rt(n,1)))

n = 1000
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

sims %.% summarise(min(signal),max(signal))
