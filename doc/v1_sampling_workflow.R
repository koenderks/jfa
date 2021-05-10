## -----------------------------------------------------------------------------
library(jfa)
data("BuildIt")

## -----------------------------------------------------------------------------
# Specify the confidence, materiality, and expected errors.
confidence    <- 0.95   # 95%
materiality   <- 0.05   # 5%
expectedError <- 0.025  # 2.5%

## -----------------------------------------------------------------------------
# Specify the inherent risk (ir) and control risk (cr).
ir <- 1     # 100%
cr <- 0.6   # 60%

## -----------------------------------------------------------------------------
# Adjust the required confidence for a frequentist analysis.
adjustedConfidence <- 1 - ((1 - confidence) / (ir * cr))
# Step 1: Calculate the required sample size.
planningResult <- planning(adjustedConfidence, materiality, expectedError = expectedError)

## -----------------------------------------------------------------------------
print(planningResult)

## -----------------------------------------------------------------------------
# Step 2: Draw a sample from the financial statements.
samplingResult <- selection(BuildIt, sampleSize = 190, units = "mus", bookValues = "bookValue")

## -----------------------------------------------------------------------------
print(samplingResult)

## -----------------------------------------------------------------------------
# Step 3: Isolate the sample for execution of the audit.
sample <- samplingResult$sample

# To write the sample to a .csv file:
# write.csv(x = sample, file = "auditSample.csv", row.names = FALSE)

# To load annotated sample back into R:
# sample <- read.csv(file = "auditSample.csv")

## -----------------------------------------------------------------------------
# Step 4: Evaluate the sample
evaluationResult <- evaluation(confidence, materiality, sample = sample, 
                               bookValues = 'bookValue', auditValues = 'auditValue')

## -----------------------------------------------------------------------------
print(evaluationResult)

