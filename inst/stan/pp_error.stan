#include /include/license.stan

data {
  int<lower=1> S;                      // number of strata
  array[S] int<lower=0> n;             // stratum sample size
  array[S] int<lower=0> k;             // stratum misstatements
  real<lower=0> alpha;                 // prior parameter alpha
  real<lower=0> beta;                  // prior parameter beta
  int beta_prior;                      // beta prior {0 = no, 1 = yes}
  int gamma_prior;                     // gamma prior {0 = no, 1 = yes}
  int normal_prior;                    // normal prior {0 = no, 1 = yes}
  int uniform_prior;                   // uniform prior {0 = no, 1 = yes}
  int cauchy_prior;                    // Cauchy prior {0 = no, 1 = yes}
  int t_prior;                         // Student-t prior {0 = no, 1 = yes}
  int chisq_prior;                     // Chi-squared prior {0 = no, 1 = yes} 
  int exponential_prior;               // exponential prior {0 = no, 1 = yes}
  int use_likelihood;                  // apply likelihood {0 = no, 1 = yes}
  int binomial_likelihood;             // binomial likelihood {0 = no, 1 = yes}
  int poisson_likelihood;              // Poisson likelihood {0 = no, 1 = yes}
}
parameters {
  real<lower=0, upper=1> phi;          // population probability of misstatement
  real<lower=1> nu;                    // population concentration
  vector<lower=0, upper=1>[S] theta_s; // stratum probability of misstatement
}
model {
  // Hyperpriors
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
  // Prior
  theta_s ~ beta(phi * nu, (1 - phi) * nu);
  // Likelihood
  if (use_likelihood) {
    if (binomial_likelihood) {
      k ~ binomial(n, theta_s);
    } else if (poisson_likelihood) {
      k ~ poisson(to_vector(n) .* theta_s);
    }
  }
}
