data {
  int<lower=0> n;
  real y[n];
}

parameters{
  real<lower=0> sigma;
  real<lower=0> tau;
  vector[n] theta;
  real<lower=0> lambda[n];
}

model {
  for (i in 1:n) {
    y[i] ~ normal(theta[i], 1);
    theta[i] ~ normal(0, lambda[i]);
    lambda[i] ~ cauchy(0,tau)T[0,];
  }

#  sigma     ~ uniform(0,100);
  tau ~ uniform(0,100);
}