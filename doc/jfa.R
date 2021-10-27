## -----------------------------------------------------------------------------
library(jfa)

data('BuildIt')
head(BuildIt, n = 10)

## -----------------------------------------------------------------------------
stage1 <- planning(materiality = 0.05, expected = 0, likelihood = 'poisson', conf.level = 0.95)
summary(stage1)

## -----------------------------------------------------------------------------
stage1 <- planning(min.precision = 0.02, expected = 0, likelihood = 'poisson', conf.level = 0.95)
summary(stage1)

## -----------------------------------------------------------------------------
set.seed(1)
stage2 <- selection(data = BuildIt, size = 60, units = 'items', method = 'random')
summary(stage2)

## -----------------------------------------------------------------------------
stage2 <- selection(data = BuildIt, size = 150, units = 'values', method = 'interval', values = 'bookValue')
summary(stage2)

## -----------------------------------------------------------------------------
set.seed(1)
stage2 <- selection(data = BuildIt, size = 60, units = 'items', method = 'random')

sample <- stage2$sample
head(sample, n = 10)

## -----------------------------------------------------------------------------
stage4 <- evaluation(materiality = 0.05, method = 'poisson', conf.level = 0.95, x = 1, n = 60)
summary(stage4)

## -----------------------------------------------------------------------------
sample$auditValue    <- sample$bookValue
sample$auditValue[1] <- sample$auditValue[1] - 100

## -----------------------------------------------------------------------------
stage4 <- evaluation(materiality = 0.05, method = 'stringer', conf.level = 0.95,
                     data = sample, values = 'bookValue', values.audit = 'auditValue',
                     times = 'times')
summary(stage4)

## ---- eval = FALSE------------------------------------------------------------
#  stage4 <- evaluation(materiality = 0.05, method = 'stringer', conf.level = 0.95,
#                       data = sample, values = 'bookValue', values.audit = 'auditValue',
#                       times = 'times')
#  
#  report(stage4, file = 'report.html', format = 'html_document') # Generates .html report

