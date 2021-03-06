data {
  int<lower=0> n;
  real y[n];
}

parameters{
  real<lower=0> sigma;
  real<lower=0> tau;
  vector[n] theta;
}

model {
  for (i in 1:n) {
    y[i] ~ normal(theta[i], 1);
    theta[i] ~ normal(0, tau);
  }

#  sigma     ~ uniform(0,100);
  tau ~ uniform(0,100);
}