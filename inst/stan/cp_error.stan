#include /include/license.stan

data {
  int<lower=1> n;
  int<lower=0> k;
  real<lower=0> alpha;
  real<lower=0> beta;
  int beta_prior;
  int gamma_prior;
  int normal_prior;
  int uniform_prior;
  int cauchy_prior;
  int t_prior;
  int chisq_prior;
  int use_likelihood;
  int binomial_likelihood;
  int poisson_likelihood;
}
parameters {
  real<lower=0, upper=1> theta;
}
model {
  if (beta_prior) {
    theta ~ beta(alpha, beta);
  } else if (gamma_prior) {
    theta ~ gamma(alpha, beta);
  } else if (normal_prior) {
    theta ~ normal(alpha, beta);
  } else if (uniform_prior) {
    theta ~ uniform(alpha, beta);
  } else if (cauchy_prior) {
    theta ~ cauchy(alpha, beta);
  } else if (t_prior) {
    theta ~ student_t(alpha, 0, 1);
  } else if (chisq_prior) {
    theta ~ chi_square(alpha);
  }
  if (use_likelihood) {
    if (binomial_likelihood) {
      k ~ binomial(n, theta);
    } else if (poisson_likelihood) {
      k ~ poisson(theta * n);
    }
  }
}
