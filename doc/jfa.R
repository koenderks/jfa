## -----------------------------------------------------------------------------
library(jfa)

data('BuildIt')
head(BuildIt, n = 10)

## -----------------------------------------------------------------------------
stage1 <- planning(confidence = 0.95, materiality = 0.05, expectedError = 0, likelihood = 'poisson', N = 3500)
summary(stage1)

## -----------------------------------------------------------------------------
stage1 <- planning(confidence = 0.95, minPrecision = 0.02, expectedError = 0, likelihood = 'poisson', N = 3500)
summary(stage1)

## -----------------------------------------------------------------------------
stage2 <- selection(population = BuildIt, sampleSize = 60, units = 'records', algorithm = 'random')
summary(stage2)

## -----------------------------------------------------------------------------
stage2 <- selection(population = BuildIt, sampleSize = 150, units = 'mus', algorithm = 'interval', bookValues = 'bookValue')
summary(stage2)

## -----------------------------------------------------------------------------
stage2 <- selection(population = BuildIt, sampleSize = 60, units = 'records', algorithm = 'random')

sample <- stage2$sample
head(sample, n = 10)

## -----------------------------------------------------------------------------
stage4 <- evaluation(confidence = 0.95,  materiality = 0.05, method = 'binomial', N = 3500, nSumstats = 60, kSumstats = 1)
summary(stage4)

## -----------------------------------------------------------------------------
sample$auditValue    <- sample$bookValue
sample$auditValue[1] <- sample$auditValue[1] - 100

## -----------------------------------------------------------------------------
stage4 <- evaluation(confidence = 0.95, materiality = 0.05, method = 'stringer', 
                     sample = sample, bookValues = 'bookValue', auditValues = 'auditValue',
                     counts = sample$count, N = 3500)
summary(stage4)

## ---- eval = FALSE------------------------------------------------------------
#  stage4 <- evaluation(confidence = 0.95, materiality = 0.05, method = 'stringer',
#                       sample = sample, bookValues = 'bookValue', auditValues = 'auditValue',
#                       counts = sample$count, N = 3500)
#  
#  report(stage4, file = 'report.html', format = 'html_document') # Generates .html report

