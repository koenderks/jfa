## -----------------------------------------------------------------------------
library(jfa)

data('BuildIt')
head(BuildIt, n = 10)

## -----------------------------------------------------------------------------
planning(confidence = 0.95, materiality = 0.05, expectedError = 0, likelihood = 'poisson', N = 3500)

## -----------------------------------------------------------------------------
planning(confidence = 0.95, minPrecision = 0.02, expectedError = 0, likelihood = 'poisson', N = 3500)

## -----------------------------------------------------------------------------
selection(population = BuildIt, sampleSize = 60, units = 'records', algorithm = 'random')

## -----------------------------------------------------------------------------
selection(population = BuildIt, sampleSize = 150, units = 'mus', algorithm = 'interval',
          bookValues = 'bookValue')

## -----------------------------------------------------------------------------
result <- selection(population = BuildIt, sampleSize = 60, units = 'records', algorithm = 'random')

sample <- result$sample
head(sample, n = 10)

## -----------------------------------------------------------------------------
evaluation(confidence = 0.95,  materiality = 0.05, method = 'binomial', N = 3500, nSumstats = 60, kSumstats = 1)

## -----------------------------------------------------------------------------
sample$auditValue    <- sample$bookValue
sample$auditValue[1] <- sample$auditValue[1] - 100

## -----------------------------------------------------------------------------
evaluation(confidence = 0.95, materiality = 0.05, method = 'stringer', sample = sample, bookValues = 'bookValue', 
           auditValues = 'auditValue', counts = sample$count, N = 3500)

## ---- eval = FALSE------------------------------------------------------------
#  result <- evaluation(confidence = 0.95, materiality = 0.05, method = 'stringer', sample = sample,
#                       bookValues = 'bookValue', auditValues = 'auditValue', counts = sample$count, N = 3500)
#  
#  report(result, file = 'report.html', format = 'html_document') # Generates .html report

