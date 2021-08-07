\name{jfa-package}
\alias{jfa}
\alias{jfa-package}
\docType{package}
\title{jfa --- Bayesian and Classical Audit Sampling}
\description{
  \if{html}{\figure{logo.png}{options: align='right' alt='logo' width='120'}}

  \code{jfa} is an \R package for statistical audit sampling. The package provides functions for planning, 
  performing, evaluating, and reporting an audit sample. Specifically, these functions implement 
  standard audit sampling techniques for calculating sample sizes, selecting items from a population, 
  and evaluating the misstatement from a data sample or from summary statistics. Additionally, 
  the \code{jfa} package allows the user to create a prior probability distribution to perform Bayesian 
  audit sampling using these functions.
  
  The package and its intended workflow are also implemented with a graphical user interface in the Audit module of 
  \href{https://jasp-stats.org}{JASP}, a free and open-source statistical software program.

  For documentation on \code{jfa} itself, including the manual and user guide
  for the package, worked examples, and other
  tutorial information visit the \href{https://koenderks.github.io/jfa/}{package website}.
  
}

\section{Reference tables}{
  Below you can find several links to reference tables that contain statistical sample sizes, upper limits, and Bayes factors. 
  These tables are created using the \code{planning()} and \code{evaluation()} functions provided in the package. See the 
  corresponding help files for more information about these functions and how to replicate this output.

  \emph{Sample sizes}
  \itemize{
  \item \href{https://github.com/koenderks/jfa/raw/development/man/figures/tables/jfaBinomialSampleSizes.pdf}{Sample sizes based on the binomial distribution}
  \item \href{https://github.com/koenderks/jfa/raw/development/man/figures/tables/jfaPoissonSampleSizes.pdf}{Sample sizes based on the Poisson distribution}
  \item \href{https://github.com/koenderks/jfa/raw/development/man/figures/tables/jfaHypergeometricSampleSizes.pdf}{Sample sizes based on the hypergeometric distribution}
  }

  \emph{Upper limits}
  \itemize{
  \item \href{https://github.com/koenderks/jfa/raw/development/man/figures/tables/jfaBinomialUpperBounds.pdf}{Upper limits based on the binomial distribution}
  \item \href{https://github.com/koenderks/jfa/raw/development/man/figures/tables/jfaPoissonUpperBounds.pdf}{Upper limits based on the Poisson distribution}
  \item \href{https://github.com/koenderks/jfa/raw/development/man/figures/tables/jfaHypergeometricUpperBounds.pdf}{Upper limits based on the hypergeometric distribution}
  }

  \emph{Bayes factors}
  \itemize{
  \item \href{https://github.com/koenderks/jfa/raw/development/man/figures/tables/jfaBinomialBayesFactors.pdf}{Bayes factors based on the beta distribution}
  \item \href{https://github.com/koenderks/jfa/raw/development/man/figures/tables/jfaPoissonBayesFactors.pdf}{Bayes factors based on the gamma distribution}
  \item \href{https://github.com/koenderks/jfa/raw/development/man/figures/tables/jfaHypergeometricBayesFactors.pdf}{Bayes factors based on the beta-binomial distribution}
  }
}

\author{
  \tabular{ll}{
    Koen Derks (maintainer, author)\tab <k.derks@nyenrode.nl> \cr
  }
 
  Please use the citation provided by \R when citing this package. 
  A BibTex entry is available from \code{citation("jfa")}.
}

\seealso{
Useful links:
\itemize{
  \item The \href{https://raw.githubusercontent.com/koenderks/jfa/development/man/figures/cheatsheet/cheatsheet.png}{cheat sheet} for a quick overview of the intended workflow.
  \item The \href{https://koenderks.github.io/jfa/articles/}{vignettes} for worked examples.
  \item The \href{https://github.com/koenderks/jfa/issues}{issue page} to submit a bug report or feature request.
}
}

\examples{

# Load the jfa package
library(jfa)

# Load the BuildIt population
data("BuildIt")

############################################
### Example 1: Classical audit sampling ####
############################################

# Stage 1: Planning
stage1 <- planning(materiality = 0.03, expectedError = 0.01, 
                   likelihood = "poisson", confidence = 0.95)
summary(stage1)

# Stage 2: Selection
stage2 <- selection(population = BuildIt, sampleSize = stage1,
                    units = "mus", bookValues = "bookValue",
                    algorithm = "interval", intervalStartingPoint = 1)
summary(stage2)

# Stage 3: Execution
sample <- stage2[["sample"]]

# Stage 4: Evaluation
stage4 <- evaluation(materiality = 0.03, method = "stringer",
                     confidence = 0.95, sample = sample,
                     bookValues = "bookValue", auditValues = "auditValue")
summary(stage4)

######################################################################
### Example 2: Bayesian audit sampling using a non-informed prior ####
######################################################################

# Create the prior distribution
prior <- auditPrior(method = "none", likelihood = "poisson")
summary(prior)

# Stage 1: Planning
stage1 <- planning(materiality = 0.03, expectedError = 0.01,
                   likelihood = "poisson", confidence = 0.95, prior = prior)
summary(stage1)

# Stage 2: Selection
stage2 <- selection(population = BuildIt, sampleSize = stage1,
                    units = "mus", bookValues = "bookValue",
                    algorithm = "interval", intervalStartingPoint = 1)
summary(stage2)

# Stage 3: Execution
sample <- stage2[["sample"]]

# Stage 4: Evaluation
stage4 <- evaluation(materiality = 0.03, confidence = 0.95, sample = sample,
                     bookValues = "bookValue", auditValues = "auditValue",
                     prior = prior)
summary(stage4)

###################################################################
### Example 3: Bayesian audit sampling using an informed prior ####
###################################################################

# Create the prior distribution
prior <- auditPrior(method = "arm", likelihood = "poisson",
                    expectedError = 0.01, materiality = 0.03, cr = 0.6)
summary(prior)

# Stage 1: Planning
stage1 <- planning(materiality = 0.03, expectedError = 0.01,
                   likelihood = "poisson", confidence = 0.95, prior = prior)
summary(stage1)

# Stage 2: Selection
stage2 <- selection(population = BuildIt, sampleSize = stage1,
                    units = "mus", bookValues = "bookValue",
                    algorithm = "interval", intervalStartingPoint = 1)
summary(stage2)

# Stage 3: Execution
sample <- stage2[["sample"]]

# Stage 4: Evaluation
stage4 <- evaluation(materiality = 0.03, confidence = 0.95, sample = sample,
                     bookValues = "bookValue", auditValues = "auditValue",
                     prior = prior)
summary(stage4)
}
\keyword{jfa} 
\keyword{package}