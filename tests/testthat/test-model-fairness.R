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
  data("compas")
  # Demographic parity
  from_jfa <- model_fairness(compas, "Ethnicity", "TwoYrRecidivism", "Predicted", reference = "Caucasian", positive = "yes")
  fairness_dp <- fairness::dem_parity(compas, outcome = "TwoYrRecidivism", preds = "Predicted", group = "Ethnicity", outcome_base = "no", base = "Caucasian")$Metric
  expect_equal(from_jfa$mle[["dp"]], as.numeric(fairness_dp[1, ]))
  expect_equal(from_jfa$mle_ratio[["dp"]], as.numeric(fairness_dp[2, ]))
  # Proportional parity
  fairness_pp <- fairness::prop_parity(compas, outcome = "TwoYrRecidivism", preds = "Predicted", group = "Ethnicity", outcome_base = "no", base = "Caucasian")$Metric
  expect_equal(from_jfa$mle[["pp"]], as.numeric(fairness_pp[1, ]))
  expect_equal(from_jfa$mle_ratio[["pp"]], as.numeric(fairness_pp[2, ]))
  # Predictive rate parity
  fairness_prp <- fairness::pred_rate_parity(compas, outcome = "TwoYrRecidivism", preds = "Predicted", group = "Ethnicity", outcome_base = "no", base = "Caucasian")$Metric
  expect_equal(from_jfa$performance[["precision"]], as.numeric(fairness_prp[1, ]))
  expect_equal(from_jfa$mle_ratio[["prp"]], as.numeric(fairness_prp[2, ]))
  # Accuracy parity
  fairness_ap <- fairness::acc_parity(compas, outcome = "TwoYrRecidivism", preds = "Predicted", group = "Ethnicity", outcome_base = "no", base = "Caucasian")$Metric
  expect_equal(from_jfa$performance[["accuracy"]], as.numeric(fairness_ap[1, ]))
  expect_equal(from_jfa$mle_ratio[["ap"]], as.numeric(fairness_ap[2, ]))
  # False negative rate parity
  fairness_fnr <- fairness::fnr_parity(compas, outcome = "TwoYrRecidivism", preds = "Predicted", group = "Ethnicity", outcome_base = "no", base = "Caucasian")$Metric
  expect_equal(from_jfa$mle[["fnrp"]], as.numeric(fairness_fnr[1, ]))
  expect_equal(from_jfa$mle_ratio[["fnrp"]], as.numeric(fairness_fnr[2, ]))
  # False positive rate parity
  fairness_fpr <- fairness::fpr_parity(compas, outcome = "TwoYrRecidivism", preds = "Predicted", group = "Ethnicity", outcome_base = "no", base = "Caucasian")$Metric
  expect_equal(from_jfa$mle[["fprp"]], as.numeric(fairness_fpr[1, ]))
  expect_equal(from_jfa$mle_ratio[["fprp"]], as.numeric(fairness_fpr[2, ]))
  # True positive rate parity
  fairness_tpr <- fairness::equal_odds(compas, outcome = "TwoYrRecidivism", preds = "Predicted", group = "Ethnicity", outcome_base = "no", base = "Caucasian")$Metric
  expect_equal(from_jfa$mle[["tprp"]], as.numeric(fairness_tpr[1, ]))
  expect_equal(from_jfa$mle_ratio[["tprp"]], as.numeric(fairness_tpr[2, ]))
  # Negative predicted value parity
  fairness_npv <- fairness::npv_parity(compas, outcome = "TwoYrRecidivism", preds = "Predicted", group = "Ethnicity", outcome_base = "no", base = "Caucasian")$Metric
  expect_equal(from_jfa$mle[["npvp"]], as.numeric(fairness_npv[1, ]))
  expect_equal(from_jfa$mle_ratio[["npvp"]], as.numeric(fairness_npv[2, ]))
  # Specificity parity
  fairness_sp <- fairness::spec_parity(compas, outcome = "TwoYrRecidivism", preds = "Predicted", group = "Ethnicity", outcome_base = "no", base = "Caucasian")$Metric
  expect_equal(from_jfa$mle[["sp"]], as.numeric(fairness_sp[1, ]))
  expect_equal(from_jfa$mle_ratio[["sp"]], as.numeric(fairness_sp[2, ]))
  # Plot
  p <- plot(from_jfa)
  expect_equal(is.null(p), FALSE)
})