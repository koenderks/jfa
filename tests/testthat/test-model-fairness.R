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
  expect_equal(from_jfa$fairness[["dp"]], as.numeric(fairness_dp[1, ]))
  expect_equal(from_jfa$ratio[["dp"]], as.numeric(fairness_dp[2, ]))
  p <- plot(from_jfa)
  expect_equal(is.null(p), FALSE)
  # Predictive rate parity
  fairness_prp <- fairness::pred_rate_parity(compas, outcome = "TwoYrRecidivism", preds = "Predicted", group = "Ethnicity", outcome_base = "no", base = "Caucasian")$Metric
  expect_equal(from_jfa$performance[["precision"]], as.numeric(fairness_prp[1, ]))
  expect_equal(from_jfa$ratio[["prp"]], as.numeric(fairness_prp[2, ]))
  # Accuracy parity
  fairness_ap <- fairness::acc_parity(compas, outcome = "TwoYrRecidivism", preds = "Predicted", group = "Ethnicity", outcome_base = "no", base = "Caucasian")$Metric
  expect_equal(from_jfa$performance[["accuracy"]], as.numeric(fairness_ap[1, ]))
  expect_equal(from_jfa$ratio[["ap"]], as.numeric(fairness_ap[2, ]))
  # False negative rate parity
  fairness_fnr <- fairness::fnr_parity(compas, outcome = "TwoYrRecidivism", preds = "Predicted", group = "Ethnicity", outcome_base = "no", base = "Caucasian")$Metric
  expect_equal(from_jfa$fairness[["fnrp"]], as.numeric(fairness_fnr[1, ]))
  expect_equal(from_jfa$ratio[["fnrp"]], as.numeric(fairness_fnr[2, ]))
  # False positive rate parity
  fairness_fpr <- fairness::fpr_parity(compas, outcome = "TwoYrRecidivism", preds = "Predicted", group = "Ethnicity", outcome_base = "no", base = "Caucasian")$Metric
  expect_equal(from_jfa$fairness[["fprp"]], as.numeric(fairness_fpr[1, ]))
  expect_equal(from_jfa$ratio[["fprp"]], as.numeric(fairness_fpr[2, ]))
  # True positive rate parity
  fairness_npv <- fairness::npv_parity(compas, outcome = "TwoYrRecidivism", preds = "Predicted", group = "Ethnicity", outcome_base = "no", base = "Caucasian")$Metric
  expect_equal(from_jfa$fairness[["npvp"]], as.numeric(fairness_npv[1, ]))
  expect_equal(from_jfa$ratio[["npvp"]], as.numeric(fairness_npv[2, ]))
})
