## -----------------------------------------------------------------------------
library(jfa)
data("BuildIt")

## -----------------------------------------------------------------------------
# Specify the confidence, materiality, and expected errors.
confidence  <- 0.95   # 95%
materiality <- 0.05   # 5%
expected    <- 0.025  # 2.5%

## -----------------------------------------------------------------------------
# Specify the inherent risk (ir) and control risk (cr).
ir <- 1     # 100%
cr <- 0.6   # 60%

## -----------------------------------------------------------------------------
# Adjust the required confidence for a frequentist analysis.
c.adj <- 1 - ((1 - confidence) / (ir * cr))
# Step 1: Calculate the required sample size.
stage1 <- planning(materiality = materiality, expected = expected, conf.level = c.adj)

## -----------------------------------------------------------------------------
summary(stage1)

## -----------------------------------------------------------------------------
# Step 2: Draw a sample from the financial statements.
stage2 <- selection(data = BuildIt, size = stage1, units = "values", values = "bookValue", method = 'interval')

## -----------------------------------------------------------------------------
summary(stage2)

## -----------------------------------------------------------------------------
# Step 3: Isolate the sample for execution of the audit.
sample <- stage2$sample

# To write the sample to a .csv file:
# write.csv(x = sample, file = "auditSample.csv", row.names = FALSE)

# To load annotated sample back into R:
# sample <- read.csv(file = "auditSample.csv")

## -----------------------------------------------------------------------------
# Step 4: Evaluate the sample
stage4 <- evaluation(materiality = materiality, conf.level = c.adj, data = sample, 
                     values = 'bookValue', values.audit = 'auditValue')

## -----------------------------------------------------------------------------
summary(stage4)

