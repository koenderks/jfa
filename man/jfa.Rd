\name{jfa-package}
\alias{jfa}
\alias{jfa-package}
\docType{package}
\title{jfa --- Statistical Methods for Auditing}
\description{
  \if{html}{\figure{logo.png}{options: alt='logo' width='120' style='float: right' }}

  \code{jfa} is an R package that provides statistical methods for auditing. The package 
  includes functions for planning, performing, evaluating, and reporting an audit sample 
  compliant with international auditing standards, as well as functions for auditing data, such as 
  testing the distribution of leading digits in the data against Benford's law. In addition to offering 
  classical frequentist methods, \code{jfa} also provides a straightforward implementation of their 
  Bayesian counterparts.
  
  The functionality of the \code{jfa} package and its intended workflow are implemented with 
  a graphical user interface in the Audit module of \href{https://jasp-stats.org}{JASP}, 
  a free and open-source software program for statistical analyses.

  For documentation on \code{jfa} itself, including the manual and user guide
  for the package, worked examples, and other tutorial information visit the 
  \href{https://koenderks.github.io/jfa/}{package website}.
  
}

\section{Reference tables}{
  Below you can find several links to reference tables that contain statistical sample sizes, upper limits, and Bayes factors. 
  These tables are created using the \code{planning()} and \code{evaluation()} functions provided in the package. See the 
  corresponding help files for more information about these functions and how to replicate this output.

  \emph{Sample sizes}
  \itemize{
  \item \href{https://github.com/koenderks/jfa/raw/development/man/figures/tables/pdf/pois.ss.pdf}{Sample sizes based on the Poisson distribution}
  \item \href{https://github.com/koenderks/jfa/raw/development/man/figures/tables/pdf/binom.ss.pdf}{Sample sizes based on the binomial distribution}
  \item \href{https://github.com/koenderks/jfa/raw/development/man/figures/tables/pdf/hyper.ss.pdf}{Sample sizes based on the hypergeometric distribution}
  }

  \emph{Upper limits}
  \itemize{
  \item \href{https://github.com/koenderks/jfa/raw/development/man/figures/tables/pdf/pois.ub.pdf}{Upper limits based on the Poisson distribution}
  \item \href{https://github.com/koenderks/jfa/raw/development/man/figures/tables/pdf/binom.ub.pdf}{Upper limits based on the binomial distribution}
  \item \href{https://github.com/koenderks/jfa/raw/development/man/figures/tables/pdf/hyper.ub.pdf}{Upper limits based on the hypergeometric distribution}
  }

  \emph{One-sided p values}
  \itemize{
  \item \href{https://github.com/koenderks/jfa/raw/development/man/figures/tables/pdf/pois.p.pdf}{One sided p values based on the Poisson distribution}
  \item \href{https://github.com/koenderks/jfa/raw/development/man/figures/tables/pdf/binom.p.pdf}{One sided p values based on the binomial distribution}
  \item \href{https://github.com/koenderks/jfa/raw/development/man/figures/tables/pdf/hyper.p.pdf}{One sided p values based on the hypergeometric distribution}
  }

  \emph{Bayes factors}
  \itemize{
  \item \href{https://github.com/koenderks/jfa/raw/development/man/figures/tables/pdf/pois.lbfi.pdf}{Impartial Bayes factors based on the gamma distribution}
  \item \href{https://github.com/koenderks/jfa/raw/development/man/figures/tables/pdf/binom.lbfi.pdf}{Impartial Bayes factors based on the beta distribution}
  \item \href{https://github.com/koenderks/jfa/raw/development/man/figures/tables/pdf/hyper.lbfi.pdf}{Impartial Bayes factors based on the beta-binomial distribution}
  }
}

\author{
  \tabular{ll}{
    Koen Derks (maintainer, author)\tab <k.derks@nyenrode.nl> \cr
  }
 
  Please use the citation provided by \R when citing this package. 
  A BibTex entry is available from \code{citation('jfa')}.
}

\seealso{
Useful links:
\itemize{
  \item The \href{https://koenderks.github.io/jfa/articles/}{vignettes} for worked examples.
  \item The \href{https://github.com/koenderks/jfa/issues}{issue page} to submit a bug report or feature request.
  \item The \href{https://raw.githubusercontent.com/koenderks/jfa/development/man/figures/cheatsheet/cheatsheet.png}{cheat sheet} for an overview of the intended audit sampling workflow.
}
}

\examples{

# Load the jfa package
library(jfa)

# Load the BuildIt population
data('BuildIt')

############################################
### Example 1: Classical audit sampling ####
############################################

# Stage 1: Planning
stage1 <- planning(materiality = 0.03, expected = 0.01, 
                   likelihood = 'poisson', conf.level = 0.95)
summary(stage1)

# Stage 2: Selection
stage2 <- selection(data = BuildIt, size = stage1,
                    units = 'values', values = 'bookValue',
                    method = 'interval', start = 1)
summary(stage2)

# Stage 3: Execution
sample <- stage2[['sample']]

# Stage 4: Evaluation
stage4 <- evaluation(materiality = 0.03, method = 'stringer',
                     conf.level = 0.95, data = sample,
                     values = 'bookValue', values.audit = 'auditValue')
summary(stage4)

######################################################################
### Example 2: Bayesian audit sampling using a non-informed prior ####
######################################################################

# Create the prior distribution
prior <- auditPrior(method = 'default', likelihood = 'poisson')
summary(prior)

# Stage 1: Planning
stage1 <- planning(materiality = 0.03, expected = 0.01,
                   likelihood = 'poisson', conf.level = 0.95, prior = prior)
summary(stage1)

# Stage 2: Selection
stage2 <- selection(data = BuildIt, size = stage1,
                    units = 'values', values = 'bookValue',
                    method = 'interval', start = 1)
summary(stage2)

# Stage 3: Execution
sample <- stage2[['sample']]

# Stage 4: Evaluation
stage4 <- evaluation(materiality = 0.03, conf.level = 0.95, data = sample,
                     values = 'bookValue', values.audit = 'auditValue',
                     prior = prior)
summary(stage4)

###################################################################
### Example 3: Bayesian audit sampling using an informed prior ####
###################################################################

# Create the prior distribution
prior <- auditPrior(method = 'arm', likelihood = 'poisson',
                    expected = 0.01, materiality = 0.03, cr = 0.6, ir = 1)
summary(prior)

# Stage 1: Planning
stage1 <- planning(materiality = 0.03, expected = 0.01,
                   likelihood = 'poisson', conf.level = 0.95, prior = prior)
summary(stage1)

# Stage 2: Selection
stage2 <- selection(data = BuildIt, size = stage1,
                    units = 'values', values = 'bookValue',
                    method = 'interval', start = 1)
summary(stage2)

# Stage 3: Execution
sample <- stage2[['sample']]

# Stage 4: Evaluation
stage4 <- evaluation(materiality = 0.03, conf.level = 0.95, data = sample,
                     values = 'bookValue', values.audit = 'auditValue',
                     prior = prior)
summary(stage4)
}
\keyword{jfa} 
\keyword{package}