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
}
}

\examples{

# Load the jfa package
library(jfa)

#################################
### Example 1: Audit sampling ###
#################################

# Load the BuildIt population
data('BuildIt')

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

#################################
### Example 2: Data auditing ####
#################################

# Load the sinoForest data set
data('sinoForest')

# Test first digits in the data against Benford's law
bl <- digit_test(sinoForest[["value"]], check = "first", reference = "benford")
summary(bl)
}
\keyword{jfa} 
\keyword{package}