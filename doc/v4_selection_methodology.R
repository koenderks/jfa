## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, fig.height=4, fig.width=6)
library(jfa)

## -----------------------------------------------------------------------------
data(BuildIt)

## -----------------------------------------------------------------------------
# Record sampling
sample <- jfa::selection(population = BuildIt, sampleSize = 100, units = 'records', algorithm = 'random')
head(sample$sample, n = 6)

## -----------------------------------------------------------------------------
# Monetary unit sampling
sample <- jfa::selection(population = BuildIt, sampleSize = 100, units = 'mus', algorithm = 'random', bookValues = 'bookValue')
head(sample$sample, n = 6)

## -----------------------------------------------------------------------------
# Record sampling
sample <- jfa::selection(population = BuildIt, sampleSize = 100, units = 'records', algorithm = 'interval', intervalStartingPoint = 1)
head(sample$sample, n = 6)

## -----------------------------------------------------------------------------
# Monetary unit sampling
sample <- jfa::selection(population = BuildIt, sampleSize = 100, units = 'mus', algorithm = 'interval', bookValues = 'bookValue', intervalStartingPoint = 1)
head(sample$sample, n = 6)

## -----------------------------------------------------------------------------
# Record sampling
sample <- jfa::selection(population = BuildIt, sampleSize = 100, units = 'records', algorithm = 'cell')
head(sample$sample, n = 6)

## -----------------------------------------------------------------------------
# Monetary unit sampling
sample <- jfa::selection(population = BuildIt, sampleSize = 100, units = 'mus', algorithm = 'cell', bookValues = 'bookValue')
head(sample$sample, n = 6)

