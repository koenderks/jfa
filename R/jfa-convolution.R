# Copyright (C) 2020-2022 Koen Derks

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program. If not, see <http://www.gnu.org/licenses/>.

#' Prior convolution utility functions
#'
#' Methods defined for convolutions of \code{jfaPrior} objects.
#'
#' @param p1  an object of class \code{jfaPrior} or class \code{jfaPosterior}.
#' @param p2  an object of class \code{jfaPrior} or class \code{jfaPosterior}.
#'
#' @return    An object of class \code{jfaPrior} or class \code{jfaPosterior}
#' constructed with method \code{convolution}.
#'
#' @name jfa-convolution
NULL

#' @rdname jfa-convolution
#' @method + jfaPrior
#' @export
"+.jfaPrior" <- function(p1, p2) {
  valid_method <- p1[["method"]] != "mcmc" && p2[["method"]] != "mcmc"
  stopifnot("method = 'mcmc' not supported" = valid_method)
  valid_likelihood <- p1[["likelihood"]] == p2[["likelihood"]]
  stopifnot("convolution not supported for different 'likelihood's" = valid_likelihood)
  likelihood <- p1[["likelihood"]]
  N.units <- p1[["N.units"]]
  materiality <- p1[["hypotheses"]]$materiality
  x <- p1[["description"]]$implicit.x + p2[["description"]]$implicit.x
  n <- p1[["description"]]$implicit.n + p2[["description"]]$implicit.n
  alpha <- 1 + x
  if (likelihood == "poisson") {
    beta <- n
  } else {
    beta <- n - x
  }
  # Initialize main results
  result <- list()
  if (inherits(p1, "jfaPrior")) {
    result[["prior"]] <- .functional_form(likelihood, alpha, beta, N.units)
  } else {
    result[["posterior"]] <- .functional_form(likelihood, alpha, beta, N.units)
  }
  # Description
  description <- list()
  description[["density"]] <- .functional_density(likelihood)
  description[["alpha"]] <- alpha
  description[["beta"]] <- beta
  description[["implicit.x"]] <- x
  description[["implicit.n"]] <- n
  result[["description"]] <- description
  # Statistics
  statistics <- list()
  statistics[["mode"]] <- .comp_mode_bayes(likelihood, alpha, beta, N.units)
  statistics[["mean"]] <- .comp_mean_bayes(likelihood, alpha, beta, N.units)
  statistics[["median"]] <- .comp_median_bayes(likelihood, alpha, beta, N.units)
  statistics[["var"]] <- .comp_var_bayes(likelihood, alpha, beta, N.units)
  statistics[["skewness"]] <- .comp_skew_bayes(likelihood, alpha, beta, N.units)
  statistics[["entropy"]] <- .comp_entropy_bayes(likelihood, alpha, beta)
  statistics[["ub"]] <- .comp_ub_bayes("less", p1[["conf.level"]], likelihood, alpha, beta, N.units)
  statistics[["precision"]] <- .comp_precision("less", statistics[["mode"]], NULL, statistics[["ub"]])
  result[["statistics"]] <- statistics
  # Hypotheses
  if (!is.null(materiality)) {
    if (inherits(p1, "jfaPosterior")) {
      alternative <- p1[["hypotheses"]]$alternative
    } else {
      alternative <- "less"
    }
    hypotheses <- list()
    hypotheses[["materiality"]] <- materiality
    hypotheses[["alternative"]] <- p1[["hypotheses"]]$alternative
    hypotheses[["hypotheses"]] <- .hyp_string(materiality, alternative)
    if (alternative == "two.sided") {
      hypotheses[["density"]] <- .hyp_dens(materiality, likelihood, alpha, beta, N.units, N.units)
    } else {
      lower_tail <- alternative == "less"
      hypotheses[["p.h1"]] <- .hyp_prob(lower_tail, materiality, likelihood, alpha, beta, N.units, N.units)
      hypotheses[["p.h0"]] <- .hyp_prob(!lower_tail, materiality, likelihood, alpha, beta, N.units, N.units)
      hypotheses[["odds.h1"]] <- hypotheses[["p.h1"]] / hypotheses[["p.h0"]]
      hypotheses[["odds.h0"]] <- 1 / hypotheses[["odds.h1"]]
    }
    result[["hypotheses"]] <- hypotheses
  }
  # Additional info
  result[["method"]] <- "convolution"
  result[["likelihood"]] <- likelihood
  result[["conf.level"]] <- p1[["conf.level"]]
  result[["N.units"]] <- N.units
  class(result) <- class(p1)
  return(result)
}

#' @rdname jfa-convolution
#' @method + jfaPosterior
#' @export
"+.jfaPosterior" <- function(p1, p2) {
  do.call(what = "+.jfaPrior", args = list(p1, p2))
}

#' @rdname jfa-convolution
#' @method * jfaPrior
#' @export
"*.jfaPrior" <- function(p1, p2) {
  p1_is_prior <- inherits(p1, "jfaPrior") || inherits(p1, "jfaPosterior")
  if (!p1_is_prior) {
    valid_const <- is.numeric(p1) && length(p1) == 1
    stopifnot("weighing requires a constant as the first argument" = valid_const)
    p2[["description"]]$w <- p1
    return(p2)
  } else {
    valid_method <- p1[["method"]] != "mcmc" && p2[["method"]] != "mcmc"
    stopifnot("method = 'mcmc' not supported" = valid_method)
    valid_likelihood <- p1[["likelihood"]] == p2[["likelihood"]]
    stopifnot("convolution not supported for different 'likelihood's" = valid_likelihood)
    likelihood <- p1[["likelihood"]]
    N.units <- p1[["N.units"]]
    materiality <- p1[["hypotheses"]]$materiality
    w <- alpha_i <- beta_i <- numeric()
    if (is.null(p1[["description"]]$w)) {
      w <- c(w, 1)
      alpha_i <- c(alpha_i, p1[["description"]]$alpha)
      beta_i <- c(beta_i, p1[["description"]]$beta)
    } else {
      if (is.null(p1[["description"]]$w_i)) {
        w <- c(w, p1[["description"]]$w)
        alpha_i <- c(alpha_i, p1[["description"]]$alpha)
        beta_i <- c(beta_i, p1[["description"]]$beta)
      } else {
        w <- c(w, p1[["description"]]$w_i)
        alpha_i <- c(alpha_i, p1[["description"]]$alpha_i)
        beta_i <- c(beta_i, p1[["description"]]$beta_i)
      }
    }
    if (is.null(p2[["description"]]$w)) {
      w <- c(w, 1)
      alpha_i <- c(alpha_i, p2[["description"]]$alpha)
      beta_i <- c(beta_i, p2[["description"]]$beta)
    } else {
      if (is.null(p2[["description"]]$w_i)) {
        w <- c(w, p2[["description"]]$w)
        alpha_i <- c(alpha_i, p2[["description"]]$alpha)
        beta_i <- c(beta_i, p2[["description"]]$beta)
      } else {
        w <- c(w, p2[["description"]]$w_i)
        alpha_i <- c(alpha_i, p2[["description"]]$alpha_i)
        beta_i <- c(beta_i, p2[["description"]]$beta_i)
      }
    }
    if (likelihood == "poisson") {
      e_x <- sum(w * (alpha_i * beta_i))
      var_x <- sum(w * (alpha_i * beta_i^2))
      alpha <- e_x^2 / var_x
      beta <- var_x / e_x
    } else {
      e_x <- sum(w * (alpha_i / (alpha_i + beta_i)))
      var_x <- sum(w^2 * ((alpha_i * beta_i) / ((alpha_i + beta_i)^2 * (alpha_i + beta_i + 1))))
      alpha <- ((e_x / sum(w))^2 * (1 - (e_x / sum(w))) / (var_x / sum(w)^2)) - (e_x / sum(w))
      beta <- ((((e_x / sum(w)) * (1 - (e_x / sum(w)))) / (var_x / sum(w)^2)) - 1) - alpha
    }
    x <- alpha - 1
    if (likelihood == "poisson") {
      n <- beta
    } else {
      n <- beta + x
    }
    # Initialize main results
    result <- list()
    if (inherits(p1, "jfaPrior")) {
      result[["prior"]] <- .functional_form(likelihood, alpha, beta, N.units)
    } else {
      result[["posterior"]] <- .functional_form(likelihood, alpha, beta, N.units)
    }
    # Description
    description <- list()
    description[["density"]] <- .functional_density(likelihood)
    description[["alpha"]] <- alpha
    description[["beta"]] <- beta
    description[["alpha_i"]] <- alpha_i
    description[["beta_i"]] <- beta_i
    description[["w_i"]] <- w
    description[["implicit.x"]] <- x
    description[["implicit.n"]] <- n
    result[["description"]] <- description
    # Statistics
    statistics <- list()
    statistics[["mode"]] <- .comp_mode_bayes(likelihood, alpha, beta, N.units)
    statistics[["mean"]] <- .comp_mean_bayes(likelihood, alpha, beta, N.units)
    statistics[["median"]] <- .comp_median_bayes(likelihood, alpha, beta, N.units)
    statistics[["var"]] <- .comp_var_bayes(likelihood, alpha, beta, N.units)
    statistics[["skewness"]] <- .comp_skew_bayes(likelihood, alpha, beta, N.units)
    statistics[["entropy"]] <- .comp_entropy_bayes(likelihood, alpha, beta)
    statistics[["ub"]] <- .comp_ub_bayes("less", p1[["conf.level"]], likelihood, alpha, beta, N.units)
    statistics[["precision"]] <- .comp_precision("less", statistics[["mode"]], NULL, statistics[["ub"]])
    result[["statistics"]] <- statistics
    # Hypotheses
    if (!is.null(materiality)) {
      if (inherits(p1, "jfaPosterior")) {
        alternative <- p1[["hypotheses"]]$alternative
      } else {
        alternative <- "less"
      }
      hypotheses <- list()
      hypotheses[["materiality"]] <- materiality
      hypotheses[["alternative"]] <- p1[["hypotheses"]]$alternative
      hypotheses[["hypotheses"]] <- .hyp_string(materiality, alternative)
      if (alternative == "two.sided") {
        hypotheses[["density"]] <- .hyp_dens(materiality, likelihood, alpha, beta, N.units, N.units)
      } else {
        lower_tail <- alternative == "less"
        hypotheses[["p.h1"]] <- .hyp_prob(lower_tail, materiality, likelihood, alpha, beta, N.units, N.units)
        hypotheses[["p.h0"]] <- .hyp_prob(!lower_tail, materiality, likelihood, alpha, beta, N.units, N.units)
        hypotheses[["odds.h1"]] <- hypotheses[["p.h1"]] / hypotheses[["p.h0"]]
        hypotheses[["odds.h0"]] <- 1 / hypotheses[["odds.h1"]]
      }
      result[["hypotheses"]] <- hypotheses
    }
    # Additional info
    result[["method"]] <- "convolution"
    result[["likelihood"]] <- likelihood
    result[["conf.level"]] <- p1[["conf.level"]]
    result[["N.units"]] <- N.units
    class(result) <- class(p1)
    return(result)
  }
}

#' @rdname jfa-convolution
#' @method * jfaPosterior
#' @export
"*.jfaPosterior" <- function(p1, p2) {
  do.call(what = "*.jfaPrior", args = list(p1, p2))
}
