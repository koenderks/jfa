#include /include/license.stan

data {
  int<lower=1> S;
  int<lower=0> n;
  array[n] int<lower=1> s;
  array[n] real<lower=0, upper=1> t;
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
}
parameters {
  real<lower=0, upper=1> phi; // population probability of misstatement
  real<lower=1> nu; // population probability of misstatement concentration
  vector<lower=0, upper=1>[S] theta_s; // stratum probability of misstatement
  real<lower=0> mu; // average stratum concentration
  real<lower=0> sigma; // stratum concentration standard deviation
  vector<lower=1>[S] kappa_s; // stratum concentration
}
model {
  if (beta_prior) {
    phi ~ beta(alpha, beta); // hyperprior
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
  nu ~ pareto(1, 1.5); // hyperprior
  mu ~ normal(1, 100); // hyperprior
  sigma ~ normal(0, 10); // hyperprior
  theta_s ~ beta(phi * nu, (1 - phi) * nu); // prior
  kappa_s ~ normal(mu, sigma); // prior
  if (use_likelihood) {
    for (i in 1:n) {
      t[i] ~ beta_proportion(theta_s[s[i]], kappa_s[s[i]]); // likelihood
    }
  }
}
