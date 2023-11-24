#include /include/license.stan

functions {
  int num_zeros(int[] y) {
    int sum = 0;
    for (i in 1:size(y))
      sum += (y[i] == 0);
    return sum;
  }
}
data {
  int<lower=0> n;                // Number of observations in sample
  int<lower=0> y[n];             // Sample differences
  int<lower=0> N;                // Number of items in population
  real<lower=0> B;               // Total book value
  real<lower=0> alpha;           // prior parameter alpha
  real<lower=0> beta;            // prior parameter beta
  int beta_prior;                // uniform prior {0 = no, 1 = yes}
  int uniform_prior;             // uniform prior {0 = no, 1 = yes}
  int gamma_prior;               // gamma prior {0 = no, 1 = yes}
  int normal_prior;              // normal prior {0 = no, 1 = yes}
  int cauchy_prior;              // Cauchy prior {0 = no, 1 = yes}
  int t_prior;                   // Student-t prior {0 = no, 1 = yes} 
  int chisq_prior;               // Chi-squared prior {0 = no, 1 = yes}
  int exponential_prior;         // exponential prior {0 = no, 1 = yes}
  int use_likelihood;            // Apply the likelihood
}
transformed data {
  int<lower = 0> N_zero = num_zeros(y);
  int<lower = 0> y_nonzero[n - N_zero];
  int N_nonzero = 0;
  for (i in 1:n) {
    if (y[i] == 0) continue;
    N_nonzero += 1;
    y_nonzero[N_nonzero] = y[i];
  }
}
parameters {
  real<lower=[(uniform_prior ? alpha : 0)][1], upper=[(uniform_prior ? beta : 1)][1]> p_error; // Probability of difference being zero
  real<lower=0, upper=(B / N)> lambda; // Average of non-zero difference
}
transformed parameters {
  real p_zero = 1 - p_error;
}
model {
  // Prior distributions
  if (beta_prior) {
    p_error ~ beta(alpha, beta);
  } else if (uniform_prior) {
    p_error ~ uniform(alpha, beta);
  } else if (gamma_prior) {
    p_error ~ gamma(alpha, beta);
  } else if (normal_prior) {
    p_error ~ normal(alpha, beta);
  } else if (cauchy_prior) {
    p_error ~ cauchy(alpha, beta);
  } else if (t_prior) {
    p_error ~ student_t(alpha, 0, 1);
  } else if (chisq_prior) {
    p_error ~ chi_square(alpha);
  } else if (exponential_prior) {
    p_error ~ exponential(alpha);
  }
  lambda ~ uniform(0, B / N);
  // Likelihood
  if (use_likelihood) { 
   target += N_zero * log_sum_exp(bernoulli_lpmf(1 | p_zero), bernoulli_lpmf(0 | p_zero) + poisson_lpmf(0 | lambda));
   target += N_nonzero * bernoulli_lpmf(0 | p_zero);
   target += poisson_lpmf(y_nonzero | lambda);
  }
}
generated quantities {
  // Posterior distribution misstatement
  real<lower=0> theta = (p_error * lambda * N) / B;
}
