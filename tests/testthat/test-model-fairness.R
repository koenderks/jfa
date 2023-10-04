# Copyright (C) 2020-2023 Koen Derks

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

context("Validation of function model_fairness")

test_that(desc = "Benchmark against fairness package", {
  testthat::skip_on_cran()
  data("compas")
  # Demographic parity
  jfa_dp <- model_fairness(compas, "Ethnicity", "TwoYrRecidivism", "Predicted", privileged = "Caucasian", positive = "yes", metric = "dp")
  fairness_dp <- fairness::dem_parity(compas, outcome = "TwoYrRecidivism", preds = "Predicted", group = "Ethnicity", outcome_base = "no", base = "Caucasian")$Metric
  expect_equal(jfa_dp[["metric"]][["all"]][["estimate"]], as.numeric(fairness_dp[1, ]))
  expect_equal(jfa_dp[["parity"]][["all"]][["estimate"]], as.numeric(fairness_dp[2, ]))
  # Proportional parity
  jfa_pp <- model_fairness(compas, "Ethnicity", "TwoYrRecidivism", "Predicted", privileged = "Caucasian", positive = "yes", metric = "pp")
  fairness_pp <- fairness::prop_parity(compas, outcome = "TwoYrRecidivism", preds = "Predicted", group = "Ethnicity", outcome_base = "no", base = "Caucasian")$Metric
  expect_equal(jfa_pp[["metric"]][["all"]][["estimate"]], as.numeric(fairness_pp[1, ]))
  expect_equal(jfa_pp[["parity"]][["all"]][["estimate"]], as.numeric(fairness_pp[2, ]))
  # Predictive rate parity
  jfa_prp <- model_fairness(compas, "Ethnicity", "TwoYrRecidivism", "Predicted", privileged = "Caucasian", positive = "yes", metric = "prp")
  fairness_prp <- fairness::pred_rate_parity(compas, outcome = "TwoYrRecidivism", preds = "Predicted", group = "Ethnicity", outcome_base = "no", base = "Caucasian")$Metric
  expect_equal(jfa_prp[["metric"]][["all"]][["estimate"]], as.numeric(fairness_prp[1, ]))
  expect_equal(jfa_prp[["parity"]][["all"]][["estimate"]], as.numeric(fairness_prp[2, ]))
  # Accuracy parity
  jfa_ap <- model_fairness(compas, "Ethnicity", "TwoYrRecidivism", "Predicted", privileged = "Caucasian", positive = "yes", metric = "ap")
  fairness_ap <- fairness::acc_parity(compas, outcome = "TwoYrRecidivism", preds = "Predicted", group = "Ethnicity", outcome_base = "no", base = "Caucasian")$Metric
  expect_equal(jfa_ap[["metric"]][["all"]][["estimate"]], as.numeric(fairness_ap[1, ]))
  expect_equal(jfa_ap[["parity"]][["all"]][["estimate"]], as.numeric(fairness_ap[2, ]))
  # False negative rate parity
  jfa_fnrp <- model_fairness(compas, "Ethnicity", "TwoYrRecidivism", "Predicted", privileged = "Caucasian", positive = "yes", metric = "fnrp")
  fairness_fnrp <- fairness::fnr_parity(compas, outcome = "TwoYrRecidivism", preds = "Predicted", group = "Ethnicity", outcome_base = "no", base = "Caucasian")$Metric
  expect_equal(jfa_fnrp[["metric"]][["all"]][["estimate"]], as.numeric(fairness_fnrp[1, ]))
  expect_equal(jfa_fnrp[["parity"]][["all"]][["estimate"]], as.numeric(fairness_fnrp[2, ]))
  # False positive rate parity
  jfa_fprp <- model_fairness(compas, "Ethnicity", "TwoYrRecidivism", "Predicted", privileged = "Caucasian", positive = "yes", metric = "fprp")
  fairness_fpr <- fairness::fpr_parity(compas, outcome = "TwoYrRecidivism", preds = "Predicted", group = "Ethnicity", outcome_base = "no", base = "Caucasian")$Metric
  expect_equal(jfa_fprp[["metric"]][["all"]][["estimate"]], as.numeric(fairness_fpr[1, ]))
  expect_equal(jfa_fprp[["parity"]][["all"]][["estimate"]], as.numeric(fairness_fpr[2, ]))
  # True positive rate parity
  jfa_tprp <- model_fairness(compas, "Ethnicity", "TwoYrRecidivism", "Predicted", privileged = "Caucasian", positive = "yes", metric = "tprp")
  fairness_tpr <- fairness::equal_odds(compas, outcome = "TwoYrRecidivism", preds = "Predicted", group = "Ethnicity", outcome_base = "no", base = "Caucasian")$Metric
  expect_equal(jfa_tprp[["metric"]][["all"]][["estimate"]], as.numeric(fairness_tpr[1, ]))
  expect_equal(jfa_tprp[["parity"]][["all"]][["estimate"]], as.numeric(fairness_tpr[2, ]))
  # Negative predicted value parity
  jfa_npvp <- model_fairness(compas, "Ethnicity", "TwoYrRecidivism", "Predicted", privileged = "Caucasian", positive = "yes", metric = "npvp")
  fairness_npv <- fairness::npv_parity(compas, outcome = "TwoYrRecidivism", preds = "Predicted", group = "Ethnicity", outcome_base = "no", base = "Caucasian")$Metric
  expect_equal(jfa_npvp[["metric"]][["all"]][["estimate"]], as.numeric(fairness_npv[1, ]))
  expect_equal(jfa_npvp[["parity"]][["all"]][["estimate"]], as.numeric(fairness_npv[2, ]))
  # Specificity parity
  jfa_sp <- model_fairness(compas, "Ethnicity", "TwoYrRecidivism", "Predicted", privileged = "Caucasian", positive = "yes", metric = "sp")
  fairness_sp <- fairness::spec_parity(compas, outcome = "TwoYrRecidivism", preds = "Predicted", group = "Ethnicity", outcome_base = "no", base = "Caucasian")$Metric
  expect_equal(jfa_sp[["metric"]][["all"]][["estimate"]], as.numeric(fairness_sp[1, ]))
  expect_equal(jfa_sp[["parity"]][["all"]][["estimate"]], as.numeric(fairness_sp[2, ]))
  # Plot
  p <- plot(jfa_sp)
  expect_equal(is.null(p), FALSE)
})
