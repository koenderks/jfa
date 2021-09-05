## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, fig.height=4, fig.width=6)
library(jfa)

## -----------------------------------------------------------------------------
data(BuildIt)

## -----------------------------------------------------------------------------
# Record sampling
set.seed(1)
sample <- selection(data = BuildIt, size = 100, units = 'rows', method = 'random')
head(sample$sample, n = 6)

## -----------------------------------------------------------------------------
# Monetary unit sampling
set.seed(1)
sample <- selection(data = BuildIt, size = 100, units = 'values', method = 'random', values = 'bookValue')
head(sample$sample, n = 6)

## -----------------------------------------------------------------------------
# Record sampling
sample <- selection(data = BuildIt, size = 100, units = 'rows', method = 'interval', start = 1)
head(sample$sample, n = 6)

## -----------------------------------------------------------------------------
# Monetary unit sampling
sample <- selection(data = BuildIt, size = 100, units = 'values', method = 'interval', values = 'bookValue', start = 1)
head(sample$sample, n = 6)

## -----------------------------------------------------------------------------
# Record sampling
set.seed(1)
sample <- selection(data = BuildIt, size = 100, units = 'rows', method = 'cell')
head(sample$sample, n = 6)

## -----------------------------------------------------------------------------
# Monetary unit sampling
set.seed(1)
sample <- selection(data = BuildIt, size = 100, units = 'values', method = 'cell', values = 'bookValue')
head(sample$sample, n = 6)

