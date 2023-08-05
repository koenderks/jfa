#include /include/license.stan

data {
  int<lower=0> y[4];
  real<lower=1> prior_a;
  int use_likelihood;
}
parameters {
  simplex[4] theta;
}
model {
  theta ~ dirichlet(rep_vector(prior_a, 4));
  if (use_likelihood) {
    y ~ multinomial(theta);
  }
}
generated quantities {
  real OR;
  real<lower=0, upper=1> prob;
  OR = (theta[1] * theta[4]) / (theta[2] * theta[3]);
  prob = theta[1] / (theta[1] + theta[2]);
}
