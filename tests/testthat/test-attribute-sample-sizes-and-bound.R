context("Appendix A - AICPA 2017")

# Sample size tables can be retrieved from: https://onlinelibrary.wiley.com/doi/pdf/10.1002/9781119448617.app1

test_that(desc = "Sample sizes for 5 percent risk of overreliance (Appendix A - AICPA 2017)", {
  expectedDeviationRate <- c(seq(0, 4, 0.25), 5:10, 12.50, 15.00, 17.50) / 100
  tolerableDeivationRate <- c(2, 3, 4, 5, 6, 7, 8, 9, 10, 15, 20) / 100
  sampleSizeMatrix <- matrix(NA, nrow = length(expectedDeviationRate), ncol = length(tolerableDeivationRate))
  rownames(sampleSizeMatrix) <- expectedDeviationRate
  colnames(sampleSizeMatrix) <- tolerableDeivationRate
  for(i in 1:length(tolerableDeivationRate)){
    for(j in 1:length(expectedDeviationRate)){
      if(i == 1 && j > 6)
        next
      if(i == 2 && j > 10)
        next
      if(i == 3 && j > 14)
        next
      if(i == 4 && j > 17)
        next
      if(i == 5 && j > 18)
        next
      if(i == 6 && j > 19)
        next
      if(i == 7 && j > 19)
        next
      if(i == 8 && j > 20)
        next
      if(i == 9 && j > 21)
        next
      if(i == 10 && j > 24)
        next
      if(i == 11 && j > 26)
        next
      jfaRes <- planning(materiality = tolerableDeivationRate[i], 
                         confidence = 0.95, 
                         expectedError = expectedDeviationRate[j], 
                         likelihood = "binomial")  
      sampleSizeMatrix[j, i] <- jfaRes$sampleSize
    }
  }
  
  aicpaMatrix <- matrix(data = c(149, 236, 313, 386, 590, 1030, rep(NA, 20), # 2%
                                 99, 157, 157, 208, 257, 303, 392, 562, 846, 1466, rep(NA, 16), # 3%
                                 74, 117, 117, 117, 156, 156, 192, 227, 294, 390, 513, 722, 1098, 1936, rep(NA, 12), # 4%
                                 59, 93, 93, 93, 93, 124, 124, 153, 181, 208, 234, 286, 361, 458, 624, 877, 1348, rep(NA, 9), #5%
                                 49, 78, 78, 78, 78, 78, 103, 103, 127, 127, 150, 173, 195, 238, 280, 341, 421, 1580, rep(NA, 8), # 6%
                                 42, 66, 66, 66, 66, 66, 66, 88, 88, 88, 109, 109, 129, 148, 167, 185, 221, 478, 1832, rep(NA, 7), # 7%
                                 36, 58, 58, 58, 58, 58, 58, 77, 77, 77, 77, 95, 95, 112, 112, 129, 146, 240, 532, rep(NA, 7), # 8%
                                 32, 51, 51, 51, 51, 51, 51, 51, 68, 68, 68, 68, 84, 84, 84, 100, 100, 158, 266, 585, rep(NA, 6), # 9%
                                 29, 46, 46, 46, 46, 46, 46, 46, 46, 61, 61, 61, 61, 61, 76, 76, 89, 116, 179, 298, 649, rep(NA, 5), # 10%
                                 19, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 40, 40, 40, 40, 50, 68, 85, 110, 150, 576, rep(NA, 2), # 15%
                                 14, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 30, 30, 37, 37, 44, 50, 88, 193, 720), # 20% 
                        nrow = length(expectedDeviationRate), ncol = length(tolerableDeivationRate), byrow = FALSE)
  rownames(aicpaMatrix) <- expectedDeviationRate
  colnames(aicpaMatrix) <- tolerableDeivationRate
  
  expect_equal(sampleSizeMatrix, 
               aicpaMatrix)
})

test_that(desc = "Sample sizes for 10 percent risk of overreliance (Appendix A - AICPA 2017)", {
  expectedDeviationRate <- c(seq(0, 4, 0.25), 5:10, 12.50, 15.00, 17.50) / 100
  tolerableDeivationRate <- c(2, 3, 4, 5, 6, 7, 8, 9, 10, 15, 20) / 100
  sampleSizeMatrix <- matrix(NA, nrow = length(expectedDeviationRate), ncol = length(tolerableDeivationRate))
  rownames(sampleSizeMatrix) <- expectedDeviationRate
  colnames(sampleSizeMatrix) <- tolerableDeivationRate
  for(i in 1:length(tolerableDeivationRate)){
    for(j in 1:length(expectedDeviationRate)){
      if(i == 1 && j > 7)
        next
      if(i == 2 && j > 10)
        next
      if(i == 3 && j > 14)
        next
      if(i == 4 && j > 17)
        next
      if(i == 5 && j > 18)
        next
      if(i == 6 && j > 19)
        next
      if(i == 7 && j > 20)
        next
      if(i == 8 && j > 21)
        next
      if(i == 9 && j > 22)
        next
      if(i == 10 && j > 24)
        next
      if(i == 11 && j > 26)
        next
      jfaRes <- planning(materiality = tolerableDeivationRate[i], 
                         confidence = 0.90, 
                         expectedError = expectedDeviationRate[j], 
                         likelihood = "binomial")  
      sampleSizeMatrix[j, i] <- jfaRes$sampleSize
    }
  }
  
  # 5 percent tolerable deviation rate and 3.5 percent expected deviation rate results in n = 423, where in AICPA (2017) it says 200. JfA is correct since the expected errors in this case are actually ceiling(14.000000000000002) = 15 and not 14.
  # 7 percent tolerable deviation rate and 6 percent expected deviation rate results in n = 1313, where in AICPA (2017) it says 1300. JfA is correct since the expected errors in this case are actually ceiling(91.000000000000014) = 92 and not 91.
  
  aicpaMatrix <- matrix(data = c(114, 194, 194, 265, 398, 708, 1463, rep(NA, 19), # 2%
                                 76, 129, 129, 129, 176, 221, 265, 390, 590, 974, rep(NA, 16), # 3%
                                 57, 96, 96, 96, 96, 132, 132, 166, 198, 262, 353, 471, 730, 1258, rep(NA, 12), # 4%
                                 45, 77, 77, 77, 77, 77, 105, 105, 132, 132, 158, 209, 258, 306, 423, 583, 873, rep(NA, 9), #5%
                                 38, 64, 64, 64, 64, 64, 64, 88, 88, 88, 110, 132, 132, 153, 194, 235, 274, 1019, rep(NA, 8), # 6%
                                 32, 55, 55, 55, 55, 55, 55, 55, 75, 75, 75, 94, 94, 113, 113, 131, 149, 318, 1150, rep(NA, 7), # 7%
                                 28, 48, 48, 48, 48, 48, 48, 48, 48, 65, 65, 65, 65, 82, 82, 98, 98, 160, 349, 1313, rep(NA, 6), # 8%
                                 25, 42, 42, 42, 42, 42, 42, 42, 42, 42, 58, 58, 58, 58, 73, 73, 73, 115, 182, 385, 1437, rep(NA, 5), # 9%
                                 22, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 52, 52, 52, 52, 52, 65, 78, 116, 199, 424, 1577, rep(NA, 4), # 10%
                                 15, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 34, 43, 52, 60, 77, 100, 368, rep(NA, 2), # 15%
                                 11, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 25, 25, 25, 32, 38, 63, 126, 457), # 20% 
                        nrow = length(expectedDeviationRate), ncol = length(tolerableDeivationRate), byrow = FALSE)
  rownames(aicpaMatrix) <- expectedDeviationRate
  colnames(aicpaMatrix) <- tolerableDeivationRate
  
  expect_equal(sampleSizeMatrix, 
               aicpaMatrix)
})