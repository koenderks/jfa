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
#' @param p1  an object of class \code{jfaPrior}.
#' @param p2  an object of class \code{jfaPrior}.
#' @param c   a numeric value, used for multiplication.
#'
#' @return    An object of class \code{jfaPrior} with method \code{convolution}.
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
  if (!is.null(p1[["N.units"]])) {
    N.units <- p1[["N.units"]]
  } else {
    N.units <- NULL
  }
  if (is.null(p1[["description"]]$w)) {
    p1[["description"]]$w <- 1
  }
  if (is.null(p2[["description"]]$w)) {
    p2[["description"]]$w <- 1
  }
  if (is.null(p1[["description"]]$alpha_i)) {
    prior_alpha_i <- c(p1[["description"]]$alpha, p2[["description"]]$alpha)
    prior_beta_i <- c(p1[["description"]]$beta, p2[["description"]]$beta)
    w <- c(p1[["description"]]$w, p2[["description"]]$w)
  } else {
    prior_alpha_i <- c(p1[["description"]]$alpha_i, p2[["description"]]$alpha)
    prior_beta_i <- c(p1[["description"]]$beta_is, p2[["description"]]$beta)
    w <- c(p1[["description"]]$w_i, p2[["description"]]$w)
  }
  if (likelihood == "poisson") {
    e_x <- sum(w * (prior_alpha_i * prior_beta_i))
    var_x <- sum(w * (prior_alpha_i * prior_beta_i^2))
    prior_alpha <- e_x^2 / var_x
    prior_beta <- var_x / e_x
  } else {
    e_x <- sum(w * (prior_alpha_i / (prior_alpha_i + prior_beta_i)))
    var_x <- sum(w^2 * ((prior_alpha_i * prior_beta_i) / ((prior_alpha_i + prior_beta_i)^2 * (prior_alpha_i + prior_beta_i + 1))))
    prior_alpha <- ((e_x / sum(w))^2 * (1 - (e_x / sum(w))) / (var_x / sum(w)^2)) - (e_x / sum(w))
    prior_beta <- ((((e_x / sum(w)) * (1 - (e_x / sum(w)))) / (var_x / sum(w)^2)) - 1) - prior_alpha
  }
  prior.x <- prior_alpha - 1
  if (likelihood == "poisson") {
    prior.n <- prior_beta
  } else {
    prior.n <- prior_beta + prior.x
  }
  # Initialize main results
  result <- list()
  result[["prior"]] <- .functional_form(likelihood, prior_alpha, prior_beta, N.units)
  # Description
  description <- list()
  description[["density"]] <- .functional_density(likelihood)
  description[["alpha"]] <- prior_alpha
  description[["beta"]] <- prior_beta
  description[["alpha_i"]] <- prior_alpha_i
  description[["beta_i"]] <- prior_beta_i
  description[["w_i"]] <- w
  description[["implicit.x"]] <- prior.x
  description[["implicit.n"]] <- prior.n
  result[["description"]] <- description
  # Statistics
  statistics <- list()
  statistics[["mode"]] <- .comp_mode_bayes(likelihood, prior_alpha, prior_beta, N.units)
  statistics[["mean"]] <- .comp_mean_bayes(likelihood, prior_alpha, prior_beta, N.units)
  statistics[["median"]] <- .comp_median_bayes(likelihood, prior_alpha, prior_beta, N.units)
  statistics[["var"]] <- .comp_var_bayes(likelihood, prior_alpha, prior_beta, N.units)
  statistics[["skewness"]] <- .comp_skew_bayes(likelihood, prior_alpha, prior_beta, N.units)
  statistics[["ub"]] <- .comp_ub_bayes("less", p1[["conf.level"]], likelihood, prior_alpha, prior_beta, N.units)
  statistics[["precision"]] <- .comp_precision("less", statistics[["mode"]], NULL, statistics[["ub"]])
  result[["statistics"]] <- statistics
  # Hypotheses
  if (!is.null(p1[["materiality"]])) {
    hypotheses <- list()
    hypotheses[["hypotheses"]] <- .hyp_string(p1[["materiality"]], "less")
    hypotheses[["p.h1"]] <- .hyp_prob(TRUE, p1[["materiality"]], likelihood, prior_alpha, prior_beta, N.units, N.units)
    hypotheses[["p.h0"]] <- .hyp_prob(FALSE, p1[["materiality"]], likelihood, prior_alpha, prior_beta, N.units, N.units)
    hypotheses[["odds.h1"]] <- hypotheses[["p.h1"]] / hypotheses[["p.h0"]]
    hypotheses[["odds.h0"]] <- 1 / hypotheses[["odds.h1"]]
    hypotheses[["density"]] <- .hyp_dens(p1[["materiality"]], likelihood, prior_alpha, prior_beta, N.units, N.units)
    result[["hypotheses"]] <- hypotheses
  }
  # Additional info
  result[["method"]] <- "convolution"
  result[["likelihood"]] <- likelihood
  if (!is.null(p1[["materiality"]])) {
    result[["materiality"]] <- p1[["materiality"]]
  }
  result[["expected"]] <- p1[["expected"]]
  result[["conf.level"]] <- p1[["conf.level"]]
  result[["N.units"]] <- N.units
  class(result) <- c(class(result), "jfaPrior")
  return(result)
}

#' @rdname jfa-convolution
#' @method * jfaPrior
#' @export
"*.jfaPrior" <- function(c, p1) {
  stopifnot("multiplication of priors not supported" = !inherits(c, "jfaPrior"))
  p1[["description"]]$w <- c
  return(p1)
}
