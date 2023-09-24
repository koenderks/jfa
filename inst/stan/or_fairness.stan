#include /include/license.stan

data {
  array[4] int<lower=0> y; // cell counts
  real<lower=1> prior_a;   // prior parameter alpha
  int use_likelihood;      // apply likelihood {0 = no, 1 = yes}
}
parameters {
  simplex[4] theta;        // concentration
}
model {
  // Prior
  theta ~ dirichlet(rep_vector(prior_a, 4));
  // Likelihood
  if (use_likelihood) {
    y ~ multinomial(theta);
  }
}
generated quantities {
  real OR;                     // odds ratio
  real<lower=0, upper=1> prob; // metric for unprivileged group
  OR = (theta[1] * theta[4]) / (theta[2] * theta[3]);
  prob = theta[1] / (theta[1] + theta[2]);
}
