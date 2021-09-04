context("Benchmark against Appendix C (AICPA 2017)")

# Audit Guide: Audit Sampling [https://future.aicpa.org/cpe-learning/publication/audit-sampling-audit-guide-OPL]
# Retrieved on 28-04-2021 from https://onlinelibrary.wiley.com/doi/pdf/10.1002/9781119448617.app3

test_that(desc = "(id f10-v0.4.0-t1) Test Monetary Unit Sample Sizes for 5 percent risk of overreliance (AICPA 2017 - Appendix C: Table C-1)", {
  riskOfIncorrectAcceptance <- c(rep(5, 6), rep(10, 5), rep(15, 5), rep(20, 5), rep(25, 5), rep(30, 4), rep(35, 4), rep(50, 4)) / 100
  ratioOfExpectedToTolerableMisstatement <- c(seq(0, 0.5, 0.1), 0, seq(0.2, 0.5, 0.1), 0, seq(0.2, 0.5, 0.1), 0, seq(0.2, 0.5, 0.1), 0, seq(0.2, 0.5, 0.1), 0, seq(0.2, 0.6, 0.2), 0, seq(0.2, 0.6, 0.2), 0, seq(0.2, 0.6, 0.2))
  tolerableMisstatement <- c(0.5, 0.3, 0.1, 0.08, 0.06, 0.05, 0.04, 0.03, 0.02, 0.01, 0.005)
  sampleSizeMatrix <- matrix(NA, nrow = length(riskOfIncorrectAcceptance), ncol = length(tolerableMisstatement)+3)
  colnames(sampleSizeMatrix) <- c("Risk", "Expected", tolerableMisstatement, "Taints")
  sampleSizeMatrix[, 1] <- riskOfIncorrectAcceptance
  sampleSizeMatrix[, 2] <- ratioOfExpectedToTolerableMisstatement
  for(i in 1:nrow(sampleSizeMatrix)){
    for(j in 1:length(tolerableMisstatement)){
      jfaRes <- planning(conf.level = (1 - sampleSizeMatrix[i, 1]), expected = tolerableMisstatement[j] * sampleSizeMatrix[i, 2], likelihood = "poisson", materiality = tolerableMisstatement[j])  
      sampleSizeMatrix[i, j+2] <- jfaRes[["n"]]   
    }
    sampleSizeMatrix[i, 14] <- ceiling(jfaRes[["x"]] * 100) / 100
  }
  
  aicpaMatrix <- matrix(data = NA, nrow = length(riskOfIncorrectAcceptance), ncol = length(tolerableMisstatement) + 3, byrow = FALSE)
  colnames(aicpaMatrix) <- c("Risk", "Expected", tolerableMisstatement, "Taints")
  aicpaMatrix[, 1] <- riskOfIncorrectAcceptance
  aicpaMatrix[, 2] <- ratioOfExpectedToTolerableMisstatement
  aicpaMatrix[, 3:14] <- matrix(c(6, 10, 30, 38, 50, 60, 75, 100, 150, 300, 600, 0,
                                  8, 13, 37, 46, 62, 74, 92, 123, 184, 368, 736, 0.37,
                                  10, 16, 47, 58, 78, 93, 116, 155, 232, 463, 925, 0.93, # For taints of highest materiality percentage AICPA gives 0.93 but correct value is 0.92000000000000004
                                  12, 20, 60, 75, 100, 120, 150, 200, 300, 600, 1199, 1.80,
                                  17, 27, 81, 102, 135, 162, 203, 270, 405, 809, 1618, 3.24,
                                  24, 39, 116, 145, 193, 231, 289, 385, 577, 1154, 2308, 5.77,
                                  5, 8, 24, 29, 39, 47, 58, 77, 116, 231, 461, 0,
                                  7, 12, 35, 43, 57, 69, 86, 114, 171, 341, 682, 0.69, # For taints of highest materiality percentage AICPA gives 0.69 but correct value is 0.68000000000000005
                                  9, 15, 44, 55, 73, 87, 109, 145, 217, 433, 866, 1.30,
                                  12, 20, 58, 72, 96, 115, 143, 191, 286, 572, 1144, 2.29,
                                  16, 27, 80, 100, 134, 160, 200, 267, 400, 799, 1597, 4.00, # For taints of highest materiality percentage AICPA gives 4 but correct value is 3.9900000000000002
                                  4, 7, 19, 24, 32, 38, 48, 64, 95, 190, 380, 0,
                                  6, 10, 28, 35, 46, 55, 69, 91, 137, 273, 545, 0.55, # For taints of highest materiality percentage AICPA gives 0.55 but correct value is 0.55000000000000004
                                  7, 12, 35, 43, 57, 69, 86, 114, 171, 341, 681, 1.03, # For taints of highest materiality percentage AICPA gives 1.03 but correct value is 1.02
                                  9, 15, 45, 56, 74, 89, 111, 148, 221, 442, 883, 1.77,
                                  13, 21, 61, 76, 101, 121, 151, 202, 302, 604, 1208, 3.02,
                                  4, 6, 17, 21, 27, 33, 41, 54, 81, 161, 322, 0,
                                  5, 8, 23, 29, 38, 46, 57, 76, 113, 226, 451, 0.46, # For taints of highest materiality percentage AICPA gives 0.46 but correct value is 0.45000000000000001
                                  6, 10, 28, 35, 47, 56, 70, 93, 139, 277, 554, 0.84, # For taints AICPA gives 0.84 but correct value is 0.82999999999999996
                                  8, 12, 36, 45, 59, 71, 89, 118, 177, 354, 707, 1.42, # For taints of highest materiality percentage AICPA gives 1.42 but correct value is 1.4099999999999999
                                  10, 16, 48, 60, 80, 95, 119, 159, 238, 475, 949, 2.38, # For taints of highest materiality percentage AICPA gives 2.28 but correct value is 2.3700000000000001
                                  3, 5, 14, 18, 24, 28, 35, 47, 70, 139, 278, 0,
                                  4, 7, 19, 24, 32, 38, 48, 64, 95, 190, 380, 0.38,
                                  5, 8, 23, 29, 39, 46, 58, 77, 115, 230, 460, 0.69,
                                  6, 10, 29, 37, 49, 58, 73, 97, 145, 289, 578, 1.16,
                                  8, 13, 38, 48, 64, 76, 95, 127, 190, 380, 760, 1.90,
                                  3, 5, 13, 16, 21, 25, 31, 41, 61, 121, 241, 0,
                                  4, 6, 17, 21, 27, 33, 41, 54, 81, 162, 323, 0.33, # For taints of highest materiality percentage AICPA gives 0.33 but correct value is 0.32000000000000001
                                  5, 8, 24, 30, 40, 48, 60, 80, 120, 239, 477, 0.96, # For taints of highest materiality percentage AICPA gives 0.96 but correct value is 0.94999999999999996
                                  9, 15, 43, 54, 71, 85, 107, 142, 213, 425, 850, 2.55,
                                  3, 4, 11, 14, 18, 21, 27, 35, 53, 105, 210, 0,
                                  3, 5, 14, 18, 23, 28, 35, 46, 69, 138, 276, 0.28, # For taints of highest materiality percentage AICPA gives 0.28 but correct value is 0.28000000000000003
                                  4, 7, 20, 25, 34, 40, 50, 67, 100, 199, 397, 0.80, # For taints of highest materiality percentage AICPA gives 0.80 but correct value is 0.79000000000000004
                                  7, 12, 34, 43, 57, 68, 85, 113, 169, 338, 676, 2.03,
                                  2, 3, 7, 9, 12, 14, 18, 24, 35, 70, 139, 0,
                                  2, 3, 9, 11, 15, 18, 22, 29, 44, 87, 173, 0.18, # For taints of highest materiality percentage AICPA gives 0.18 but correct value is 0.17000000000000001
                                  3, 4, 12, 15, 19, 23, 29, 38, 57, 114, 228, 0.46,
                                  4, 6, 17, 22, 29, 34, 43, 57, 85, 170, 340, 1.02),
                                ncol = length(tolerableMisstatement) + 1, nrow = length(riskOfIncorrectAcceptance), byrow = TRUE)
  
  expect_equal(sampleSizeMatrix, aicpaMatrix, tolerance = 0.01)
})
