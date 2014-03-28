data {
  int<lower=0> n;
  real y[n];
  real<lower=0> df;
}

parameters{
  real<lower=0> sigma;
  real<lower=0> tau;
  vector[n] theta;
  real<lower=0> lambda2[n];
}

model {
  for (i in 1:n) {
    y[i] ~ normal(theta[i], 1);
    theta[i] ~ normal(0, sqrt(lambda2[i]));
    lambda2[i] ~ gamma(df/2, df*pow(tau,2)/2);
  }

#  sigma     ~ uniform(0,100);
  tau ~ uniform(0,100);
}