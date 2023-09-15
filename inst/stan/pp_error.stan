#include /include/license.stan

data {
  int<lower=1> S;
  array[S] int<lower=0> n;
  array[S] int<lower=0> k;
  real<lower=0> alpha;
  real<lower=0> beta;
  int beta_prior;
  int gamma_prior;
  int normal_prior;
  int uniform_prior;
  int cauchy_prior;
  int t_prior;
  int chisq_prior;
  int exponential_prior;
  int use_likelihood;
  int binomial_likelihood;
  int poisson_likelihood;
}
parameters {
  real<lower=0, upper=1> phi;
  real<lower=0> nu;
  vector<lower=0, upper=1>[S] theta_s;
}
model {
  if (beta_prior) {
    phi ~ beta(alpha, beta);
  } else if (gamma_prior) {
    phi ~ gamma(alpha, beta);
  } else if (normal_prior) {
    phi ~ normal(alpha, beta);
  } else if (uniform_prior) {
    phi ~ uniform(alpha, beta);
  } else if (cauchy_prior) {
    phi ~ cauchy(alpha, beta);
  } else if (t_prior) {
    phi ~ student_t(alpha, 0, 1);
  } else if (chisq_prior) {
    phi ~ chi_square(alpha);
  } else if (exponential_prior) {
    phi ~ exponential(alpha);
  }
  nu ~ pareto(1, 1.5);
  theta_s ~ beta_proportion(phi, nu);
  if (use_likelihood) {
    if (binomial_likelihood) {
      k ~ binomial(n, theta_s);
    } else if (poisson_likelihood) {
      for (i in 1:S) {
        k[i] ~ poisson(n[i] * theta_s[i]);
      }
    }
  }
}
