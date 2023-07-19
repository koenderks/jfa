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

test_that(desc = "Compas data", {
  data("compas")
  res <- model_fairness(compas, "Ethnicity", "TwoYrRecidivism", "Predicted", reference = "Caucasian")
  expect_equal(res$fairness, structure(list(group = c(
    "Caucasian",
    "African_American", "Asian", "Hispanic", "Native_American", "Other"
  ), dp = c(822L, 1661L, 8L, 189L, 5L, 124L), pp = c(
    0.390870185449358,
    0.523149606299213, 0.258064516129032, 0.371316306483301, 0.454545454545455,
    0.361516034985423
  ), prp = c(
    0.472019464720195, 0.752558699578567,
    0.25, 0.465608465608466, 0.6, 0.419354838709677
  ), ap = c(
    0.658582976699952,
    0.67244094488189, 0.741935483870968, 0.681728880157171, 0.636363636363636,
    0.693877551020408
  ), fnrp = c(
    0.422619047619048, 0.334752527940394,
    0.5, 0.409395973154362, 0.4, 0.388235294117647
  ), fprp = c(
    0.303284416491964,
    0.31712962962963, 0.222222222222222, 0.280555555555556, 0.333333333333333,
    0.27906976744186
  ), tprp = c(
    0.577380952380952, 0.665247472059606,
    0.5, 0.590604026845638, 0.6, 0.611764705882353
  ), npvp = c(
    0.778298204527713,
    0.584544253632761, 0.91304347826087, 0.809375, 0.666666666666667,
    0.849315068493151
  ), sp = c(
    0.696715583508036, 0.68287037037037,
    0.777777777777778, 0.719444444444444, 0.666666666666667, 0.72093023255814
  )), row.names = c(NA, -6L), class = "data.frame"))
  expect_equal(res$ratio, structure(list(group = c(
    "Caucasian", "African_American",
    "Asian", "Hispanic", "Native_American", "Other"
  ), dp = c(
    1, 2.02068126520681,
    0.0097323600973236, 0.22992700729927, 0.00608272506082725, 0.150851581508516
  ), pp = c(
    1, 1.33842289786769, 0.660230751118436, 0.949973470236473,
    1.16290643662906, 0.924900512864165
  ), prp = c(
    1, 1.59433827591129,
    0.529639175257732, 0.986417934871543, 1.27113402061856, 0.888427003658131
  ), ap = c(
    1, 1.0210420989795, 1.12656340980552, 1.03514500719894,
    0.966261896947818, 1.05359168938333
  ), fnrp = c(
    1, 0.792090488647692,
    1.1830985915493, 0.968711598449759, 0.946478873239437, 0.91864125932063
  ), fprp = c(
    1, 1.04565092165899, 0.732718894009217, 0.925057603686636,
    1.09907834101382, 0.920158611081342
  ), tprp = c(
    1, 1.15218118872179,
    0.865979381443299, 1.02290181969141, 1.03917525773196, 1.05955124317768
  ), npvp = c(
    1, 0.751054351959445, 1.17312807989185, 1.03992916248746,
    0.856569709127382, 1.09124634176502
  ), sp = c(
    1, 0.980127883650953,
    1.11634904714142, 1.03262286860582, 0.956870611835506, 1.03475542907793
  ), deviation = c(0L, 5L, 4L, 1L, 2L, 1L)), class = "data.frame", row.names = c(
    NA,
    -6L
  )))
  expect_equal(res$performance, structure(list(group = c(
    "Caucasian",
    "African_American", "Asian", "Hispanic", "Native_American", "Other"
  ), accuracy = c(
    0.658582976699952, 0.67244094488189, 0.741935483870968,
    0.681728880157171, 0.636363636363636, 0.693877551020408
  ), precision = c(
    0.472019464720195,
    0.752558699578567, 0.25, 0.465608465608466, 0.6, 0.419354838709677
  ), recall = c(
    0.577380952380952, 0.665247472059606, 0.5, 0.590604026845638,
    0.6, 0.611764705882353
  ), f1.score = c(
    0.519410977242303, 0.706214689265537,
    0.333333333333333, 0.520710059171598, 0.6, 0.497607655502392
  )), row.names = c(
    NA,
    -6L
  ), class = "data.frame"))
  p <- plot(res)
  expect_equal(is.null(p), FALSE)
})
