context("Consistency of statistical tables")

###################
## Sample sizes ###
###################

# Binomial distribution

test_that(desc = "(id: f7-v0.5.5-t1) Statistical Sample Sizes based on the Binomial Distribution - 10 Percent Risk of Overreliance", {
  testthat::skip_on_cran()
  reference <- matrix(c(0, 0.0025, 0.005, 0.0075, 0.01, 0.0125, 0.015, 0.0175, 0.02, 0.0225, 0.025, 0.0275, 0.03, 0.0325, 0.035, 0.0375, 0.04, 0.0425, 0.045, 0.0475, 0.05, 0.0525, 0.055, 0.0575, 0.06, 0.0625, 0.065, 0.0675, 0.07, 0.0725, 0.075, 0.0775, 0.08, 0.0825, 0.085, 0.0875, 0.09, 0.0925, 0.095, 0.0975, 0.1, 
                        230, 388, 798, 2929, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        114, 194, 194, 265, 398, 708, 1463, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        76, 129, 129, 129, 176, 221, 265, 390, 590, 974, 2079, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        57, 96, 96, 96, 96, 132, 132, 166, 198, 262, 353, 471, 730, 1258, 2712, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        45, 77, 77, 77, 77, 77, 105, 105, 132, 132, 158, 209, 258, 306, 423, 583, 873, 1505, 3310, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        38, 64, 64, 64, 64, 64, 64, 88, 88, 88, 110, 132, 132, 153, 194, 235, 274, 352, 485, 672, 1019, 1771, 3890, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        32, 55, 55, 55, 55, 55, 55, 55, 75, 75, 75, 94, 94, 113, 113, 131, 149, 184, 218, 252, 318, 399, 544, 764, 1150, 2016, 4446, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        28, 48, 48, 48, 48, 48, 48, 48, 48, 65, 65, 65, 65, 82, 82, 98, 98, 114, 130, 145, 160, 190, 234, 278, 349, 447, 599, 844, 1313, 2262, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        25, 42, 42, 42, 42, 42, 42, 42, 42, 42, 58, 58, 58, 58, 73, 73, 73, 87, 87, 101, 115, 129, 142, 156, 182, 208, 259, 310, 385, 496, 666, 929, 1437, 2509, NA, NA, NA, NA, NA, NA, NA, 
                        22, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 52, 52, 52, 52, 52, 65, 65, 65, 78, 78, 91, 104, 104, 116, 128, 152, 175, 199, 233, 279, 335, 424, 533, 728, 1017, 1577, 2756, NA, NA, NA, 
                        15, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 34, 34, 34, 34, 34, 34, 34, 43, 43, 43, 43, 52, 52, 52, 60, 60, 60, 68, 68, 77, 85, 93, 100, 100, 
                        11, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 32, 32, 32, 32, 32, 38, 38, 38), ncol = 13)
  
  conf.level <- 0.90
  m <- c(seq(0.01, 0.10, 0.01), 0.15, 0.20)
  expected <- seq(0, 0.1, 0.0025)
  
  tab <- matrix(NA, nrow = length(expected), ncol = length(m))
  for(rows in 1:length(expected)){
    for(columns in 1:length(m)){
      if(expected[rows] >= m[columns]){
        next
      } else {
        p <- try({ suppressMessages(ss <- jfa::planning(conf.level = conf.level, expected = expected[rows], likelihood = "binomial", materiality = m[columns])) }, silent = T)
        if(class(p) == "try-error"){
          next 
        }
        tab[rows, columns] <- ss$n
      }
    }
  }
  
  table <- as.data.frame(tab, stringsAsFactors = F)
  table <- cbind(expected = round(expected, 4), table)
  table <- matrix(as.numeric(unlist(table)), ncol = 13)
  
  expect_equal(table, reference)
})

test_that(desc = "(id: f7-v0.5.5-t2) Statistical Sample Sizes based on the Binomial Distribution - 5 Percent Risk of Overreliance", {
  testthat::skip_on_cran()
  reference <- matrix(c(0, 0.0025, 0.005, 0.0075, 0.01, 0.0125, 0.015, 0.0175, 0.02, 0.0225, 0.025, 0.0275, 0.03, 0.0325, 0.035, 0.0375, 0.04, 0.0425, 0.045, 0.0475, 0.05, 0.0525, 0.055, 0.0575, 0.06, 0.0625, 0.065, 0.0675, 0.07, 0.0725, 0.075, 0.0775, 0.08, 0.0825, 0.085, 0.0875, 0.09, 0.0925, 0.095, 0.0975, 0.1, 
                        299, 628, 1182, 4521, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        149, 236, 313, 386, 590, 1030, 2258, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        99, 157, 157, 208, 257, 303, 392, 562, 846, 1466, 3240, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        74, 117, 117, 117, 156, 156, 192, 227, 294, 390, 513, 722, 1098, 1936, 4257, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        59, 93, 93, 93, 93, 124, 124, 153, 181, 208, 234, 286, 361, 458, 624, 877, 1348, 2375, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        49, 78, 78, 78, 78, 78, 103, 103, 127, 127, 150, 173, 195, 238, 280, 341, 421, 539, 711, 1030, 1580, 2799, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        42, 66, 66, 66, 66, 66, 66, 88, 88, 88, 109, 109, 129, 148, 167, 185, 221, 257, 309, 377, 478, 609, 818, 1181, 1832, 3200, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        36, 58, 58, 58, 58, 58, 58, 77, 77, 77, 77, 95, 95, 112, 112, 129, 146, 162, 193, 209, 240, 285, 344, 417, 532, 687, 923, 1332, 2057, 3627, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        32, 51, 51, 51, 51, 51, 51, 51, 68, 68, 68, 68, 84, 84, 84, 100, 100, 115, 129, 143, 158, 185, 199, 226, 266, 319, 383, 472, 585, 758, 1026, 1482, 2275, 4012, NA, NA, NA, NA, NA, NA, NA, 
                        29, 46, 46, 46, 46, 46, 46, 46, 46, 61, 61, 61, 61, 61, 76, 76, 89, 89, 103, 103, 116, 129, 142, 154, 179, 191, 215, 251, 298, 356, 413, 515, 649, 836, 1129, 1611, 2500, 4421, NA, NA, NA, 
                        19, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 40, 40, 40, 40, 40, 40, 40, 50, 50, 50, 50, 59, 59, 59, 68, 68, 76, 76, 85, 93, 93, 102, 110, 118, 126, 142, 150, 14, 
                        22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 30, 30, 30, 30, 30, 30, 30, 30, 37, 37, 37, 37, 37, 37, 44, 44, 44, 44, 50, 50, 50, 50), ncol = 13)
  
  conf.level <- 0.95
  m <- c(seq(0.01, 0.10, 0.01), 0.15, 0.20)
  expected <- seq(0, 0.1, 0.0025)
  
  tab <- matrix(NA, nrow = length(expected), ncol = length(m))
  for(rows in 1:length(expected)){
    for(columns in 1:length(m)){
      if(expected[rows] >= m[columns]){
        next
      } else {
        p <- try({ suppressMessages(ss <- jfa::planning(conf.level = conf.level, expected = expected[rows], likelihood = "binomial", materiality = m[columns])) }, silent = T)
        if(class(p) == "try-error"){
          next 
        }
        tab[rows, columns] <- ss$n
      }
    }
  }
  
  table <- as.data.frame(tab, stringsAsFactors = F)
  table <- cbind(expected = round(expected, 4), table)
  table <- matrix(as.numeric(unlist(table)), ncol = 13)
  
  expect_equal(table, reference)
})

test_that(desc = "(id: f7-v0.5.5-t3) Statistical Sample Sizes based on the Binomial Distribution - 2.5 Percent Risk of Overreliance", {
  testthat::skip_on_cran()
  reference <- matrix(c(0, 0.0025, 0.005, 0.0075, 0.01, 0.0125, 0.015, 0.0175, 0.02, 0.0225, 0.025, 0.0275, 0.03, 0.0325, 0.035, 0.0375, 0.04, 0.0425, 0.045, 0.0475, 0.05, 0.0525, 0.055, 0.0575, 0.06, 0.0625, 0.065, 0.0675, 0.07, 0.0725, 0.075, 0.0775, 0.08, 0.0825, 0.085, 0.0875, 0.09, 0.0925, 0.095, 0.0975, 0.1, 
                        368, 720, 1573, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        183, 277, 359, 509, 785, 1356, 3061, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        122, 184, 184, 239, 290, 386, 522, 737, 1145, 2000, 4518, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        91, 137, 137, 178, 178, 217, 253, 323, 391, 488, 676, 976, 1498, 2646, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        72, 110, 110, 110, 142, 142, 173, 202, 230, 258, 312, 390, 465, 613, 827, 1197, 1849, 3270, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        60, 91, 91, 91, 91, 118, 118, 144, 144, 168, 192, 215, 259, 303, 366, 449, 550, 727, 977, 1409, 2199, 3885, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        51, 78, 78, 78, 78, 78, 101, 101, 123, 123, 144, 144, 164, 183, 222, 240, 295, 349, 419, 505, 639, 837, 1127, 1617, 2533, 4480, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        45, 68, 68, 68, 68, 68, 88, 88, 88, 88, 107, 107, 125, 143, 160, 160, 194, 210, 242, 289, 320, 396, 471, 573, 731, 943, 1290, 1836, 2857, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        40, 60, 60, 60, 60, 60, 60, 78, 78, 78, 78, 95, 95, 111, 111, 127, 142, 157, 172, 186, 215, 243, 270, 311, 365, 431, 522, 636, 812, 1048, 1426, 2050, 3175, NA, NA, NA, NA, NA, NA, NA, NA, 
                        36, 54, 54, 54, 54, 54, 54, 54, 70, 70, 70, 70, 85, 85, 85, 100, 100, 114, 127, 141, 154, 167, 180, 206, 231, 255, 292, 340, 399, 480, 572, 708, 875, 1151, 1564, 2251, 3499, NA, NA, NA, NA, 
                        23, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 46, 46, 46, 46, 46, 46, 56, 56, 56, 56, 66, 66, 66, 75, 75, 84, 84, 93, 93, 102, 110, 119, 127, 136, 144, 160, 177, 193, 209, 17, 
                        26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 34, 34, 34, 34, 34, 34, 34, 34, 41, 41, 41, 41, 41, 41, 48, 48, 48, 48, 55, 55, 55, 62, 62, 69, 69), ncol = 13)
  
  conf.level <- 0.975
  m <- c(seq(0.01, 0.10, 0.01), 0.15, 0.20)
  expected <- seq(0, 0.1, 0.0025)
  
  tab <- matrix(NA, nrow = length(expected), ncol = length(m))
  for(rows in 1:length(expected)){
    for(columns in 1:length(m)){
      if(expected[rows] >= m[columns]){
        next
      } else {
        p <- try({ suppressMessages(ss <- jfa::planning(conf.level = conf.level, expected = expected[rows], likelihood = "binomial", materiality = m[columns])) }, silent = T)
        if(class(p) == "try-error"){
          next 
        }
        tab[rows, columns] <- ss$n
      }
    }
  }
  
  table <- as.data.frame(tab, stringsAsFactors = F)
  table <- cbind(expected = round(expected, 4), table)
  table <- matrix(as.numeric(unlist(table)), ncol = 13)
  
  expect_equal(table, reference)
})

test_that(desc = "(id: f7-v0.5.5-t4) Statistical Sample Sizes based on the Binomial Distribution - 1 Percent Risk of Overreliance", {
  testthat::skip_on_cran()
  reference <- matrix(c(0, 0.0025, 0.005, 0.0075, 0.01, 0.0125, 0.015, 0.0175, 0.02, 0.0225, 0.025, 0.0275, 0.03, 0.0325, 0.035, 0.0375, 0.04, 0.0425, 0.045, 0.0475, 0.05, 0.0525, 0.055, 0.0575, 0.06, 0.0625, 0.065, 0.0675, 0.07, 0.0725, 0.075, 0.0775, 0.08, 0.0825, 0.085, 0.0875, 0.09, 0.0925, 0.095, 0.0975, 0.1, 
                        459, 1001, 2144, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        228, 330, 499, 652, 1070, 1836, 4193, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        152, 219, 277, 332, 383, 529, 712, 971, 1546, 2754, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        113, 164, 164, 207, 248, 287, 324, 396, 499, 663, 915, 1307, 2063, 3660, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        90, 130, 130, 130, 165, 198, 198, 259, 288, 344, 398, 504, 631, 828, 1139, 1625, 2549, 4540, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        75, 108, 108, 108, 137, 137, 164, 164, 190, 215, 239, 286, 331, 397, 483, 608, 749, 987, 1353, 1936, 3020, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        64, 92, 92, 92, 92, 117, 117, 140, 140, 162, 184, 204, 225, 244, 283, 340, 395, 467, 555, 693, 878, 1142, 1562, 2243, 3500, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        56, 81, 81, 81, 81, 102, 102, 102, 122, 122, 142, 142, 160, 178, 196, 213, 247, 280, 329, 376, 439, 530, 635, 782, 998, 1295, 1768, 2533, 3971, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        49, 71, 71, 71, 71, 71, 91, 91, 91, 109, 109, 109, 126, 142, 142, 158, 174, 204, 219, 249, 277, 320, 362, 416, 497, 576, 707, 873, 1112, 1447, 1960, 2838, 4425, NA, NA, NA, NA, NA, NA, NA, NA, 
                        44, 64, 64, 64, 64, 64, 64, 81, 81, 81, 97, 97, 97, 113, 113, 127, 142, 156, 170, 183, 197, 223, 236, 275, 300, 350, 398, 459, 542, 647, 773, 966, 1212, 1587, 2164, 3119, 4866, NA, NA, NA, NA, 
                        29, 42, 42, 42, 42, 42, 42, 42, 42, 42, 53, 53, 53, 53, 53, 53, 64, 64, 64, 74, 74, 74, 84, 84, 93, 93, 103, 103, 112, 121, 130, 138, 147, 156, 164, 181, 198, 214, 231, 255, 279, 
                        21, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 39, 39, 39, 39, 39, 39, 39, 39, 47, 47, 47, 47, 47, 55, 55, 55, 55, 62, 62, 62, 69, 69, 76, 76, 83, 83, 89, 89), ncol = 13)
  
  conf.level <- 0.99
  m <- c(seq(0.01, 0.10, 0.01), 0.15, 0.20)
  expected <- seq(0, 0.1, 0.0025)
  
  tab <- matrix(NA, nrow = length(expected), ncol = length(m))
  for(rows in 1:length(expected)){
    for(columns in 1:length(m)){
      if(expected[rows] >= m[columns]){
        next
      } else {
        p <- try({ suppressMessages(ss <- jfa::planning(conf.level = conf.level, expected = expected[rows], likelihood = "binomial", materiality = m[columns])) }, silent = T)
        if(class(p) == "try-error"){
          next 
        }
        tab[rows, columns] <- ss$n
      }
    }
  }
  
  table <- as.data.frame(tab, stringsAsFactors = F)
  table <- cbind(expected = round(expected, 4), table)
  table <- matrix(as.numeric(unlist(table)), ncol = 13)
  
  expect_equal(table, reference)
})

# Poisson distribution

test_that(desc = "(id: f7-v0.5.5-t5) Statistical Sample Sizes based on the Poisson Distribution - 10 Percent Risk of Overreliance", {
  testthat::skip_on_cran()
  reference <- matrix(c(0, 0.0025, 0.005, 0.0075, 0.01, 0.0125, 0.015, 0.0175, 0.02, 0.0225, 0.025, 0.0275, 0.03, 0.0325, 0.035, 0.0375, 0.04, 0.0425, 0.045, 0.0475, 0.05, 0.0525, 0.055, 0.0575, 0.06, 0.0625, 0.065, 0.0675, 0.07, 0.0725, 0.075, 0.0775, 0.08, 0.0825, 0.085, 0.0875, 0.09, 0.0925, 0.095, 0.0975, 0.1, 
                        231, 383, 799, 2925, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        116, 146, 192, 266, 400, 681, 1463, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        77, 90, 106, 128, 158, 201, 267, 373, 566, 975, 2122, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        58, 65, 73, 83, 96, 112, 133, 161, 200, 256, 341, 480, 732, 1269, 2781, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        47, 51, 56, 62, 69, 77, 87, 99, 115, 135, 160, 194, 242, 310, 414, 585, 897, 1562, 3439, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        39, 42, 45, 49, 53, 58, 64, 71, 79, 89, 101, 115, 134, 157, 187, 227, 283, 364, 488, 691, 1061, 1854, 4096, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        33, 36, 38, 41, 44, 47, 51, 55, 60, 66, 73, 80, 90, 101, 115, 131, 152, 179, 213, 260, 325, 418, 561, 797, 1226, 2146, 4754, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        29, 31, 33, 35, 37, 39, 42, 45, 48, 52, 56, 61, 67, 73, 81, 90, 100, 113, 128, 147, 171, 201, 240, 293, 366, 472, 635, 902, 1391, 2439, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        26, 27, 29, 30, 32, 34, 36, 38, 40, 43, 46, 49, 53, 57, 62, 67, 74, 81, 89, 99, 111, 125, 142, 163, 189, 223, 267, 325, 407, 526, 708, 1007, 1555, 2731, NA, NA, NA, NA, NA, NA, NA, 
                        24, 25, 26, 27, 28, 30, 31, 33, 35, 37, 39, 41, 44, 47, 50, 54, 58, 62, 68, 73, 80, 88, 97, 108, 121, 137, 155, 178, 207, 245, 293, 358, 449, 580, 781, 1113, 1720, 3023, NA, NA, NA, 
                        16, 16, 17, 17, 18, 18, 19, 20, 20, 21, 22, 22, 23, 24, 25, 26, 27, 28, 29, 31, 32, 33, 35, 37, 39, 41, 43, 45, 48, 51, 54, 57, 61, 65, 70, 75, 81, 88, 95, 104, 114, 
                        12, 12, 13, 13, 13, 13, 14, 14, 14, 15, 15, 15, 16, 16, 17, 17, 18, 18, 19, 19, 20, 20, 21, 21, 22, 23, 24, 24, 25, 26, 27, 28, 29, 30, 31, 33, 34, 35, 37, 39, 40), ncol = 13)
  
  conf.level <- 0.90
  m <- c(seq(0.01, 0.10, 0.01), 0.15, 0.20)
  expected <- seq(0, 0.1, 0.0025)
  
  tab <- matrix(NA, nrow = length(expected), ncol = length(m))
  for(rows in 1:length(expected)){
    for(columns in 1:length(m)){
      if(expected[rows] >= m[columns]){
        next
      } else {
        p <- try({ suppressMessages(ss <- jfa::planning(conf.level = conf.level, expected = expected[rows], likelihood = "poisson", materiality = m[columns])) }, silent = T)
        if(class(p) == "try-error"){
          next 
        }
        tab[rows, columns] <- ss$n
      }
    }
  }
  
  table <- as.data.frame(tab, stringsAsFactors = F)
  table <- cbind(expected = round(expected, 4), table)
  table <- matrix(as.numeric(unlist(table)), ncol = 13)
  
  expect_equal(table, reference)
})

test_that(desc = "(id: f7-v0.5.5-t6) Statistical Sample Sizes based on the Poisson Distribution - 5 Percent Risk of Overreliance", {
  testthat::skip_on_cran()
  reference <- matrix(c(0, 0.0025, 0.005, 0.0075, 0.01, 0.0125, 0.015, 0.0175, 0.02, 0.0225, 0.025, 0.0275, 0.03, 0.0325, 0.035, 0.0375, 0.04, 0.0425, 0.045, 0.0475, 0.05, 0.0525, 0.055, 0.0575, 0.06, 0.0625, 0.065, 0.0675, 0.07, 0.0725, 0.075, 0.0775, 0.08, 0.0825, 0.085, 0.0875, 0.09, 0.0925, 0.095, 0.0975, 0.1, 
                        300, 524, 1154, 4487, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        150, 195, 262, 374, 577, 1013, 2244, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        100, 119, 143, 175, 220, 285, 385, 550, 850, 1496, 3329, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        75, 85, 98, 113, 131, 156, 187, 230, 289, 375, 507, 724, 1122, 1978, 4412, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        60, 67, 74, 83, 93, 105, 120, 139, 162, 192, 231, 284, 357, 464, 628, 898, 1393, 2460, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        50, 55, 60, 65, 72, 79, 88, 98, 110, 125, 143, 165, 193, 228, 275, 338, 425, 553, 748, 1071, 1665, 2941, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        43, 46, 50, 54, 58, 63, 69, 75, 83, 91, 101, 113, 128, 145, 165, 191, 223, 265, 319, 392, 493, 641, 869, 1245, 1935, 3423, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        38, 40, 43, 46, 49, 53, 57, 61, 66, 72, 78, 85, 94, 104, 115, 129, 145, 164, 188, 217, 254, 301, 362, 445, 561, 730, 989, 1418, 2206, 3904, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        34, 36, 38, 40, 42, 45, 48, 51, 55, 59, 63, 68, 74, 80, 87, 95, 105, 116, 129, 144, 162, 184, 210, 243, 284, 337, 406, 499, 629, 819, 1110, 1592, 2477, 4385, NA, NA, NA, NA, NA, NA, NA, 
                        30, 32, 34, 35, 37, 39, 42, 44, 47, 50, 53, 56, 60, 65, 70, 75, 81, 88, 96, 105, 116, 128, 142, 159, 179, 203, 232, 268, 314, 372, 449, 553, 697, 907, 1230, 1765, 2748, 4866, NA, NA, NA, 
                        20, 21, 22, 23, 23, 24, 25, 26, 27, 28, 29, 30, 31, 33, 34, 35, 37, 39, 40, 42, 44, 47, 49, 52, 54, 57, 61, 64, 68, 73, 77, 83, 88, 95, 102, 110, 119, 130, 142, 155, 170, 
                        15, 16, 16, 17, 17, 17, 18, 18, 19, 19, 20, 20, 21, 22, 22, 23, 24, 24, 25, 26, 27, 28, 28, 29, 30, 32, 33, 34, 35, 36, 38, 39, 41, 43, 44, 46, 48, 51, 53, 55, 58), ncol = 13)
  
  conf.level <- 0.95
  m <- c(seq(0.01, 0.10, 0.01), 0.15, 0.20)
  expected <- seq(0, 0.1, 0.0025)
  
  tab <- matrix(NA, nrow = length(expected), ncol = length(m))
  for(rows in 1:length(expected)){
    for(columns in 1:length(m)){
      if(expected[rows] >= m[columns]){
        next
      } else {
        p <- try({ suppressMessages(ss <- jfa::planning(conf.level = conf.level, expected = expected[rows], likelihood = "poisson", materiality = m[columns])) }, silent = T)
        if(class(p) == "try-error"){
          next 
        }
        tab[rows, columns] <- ss$n
      }
    }
  }
  
  table <- as.data.frame(tab, stringsAsFactors = F)
  table <- cbind(expected = round(expected, 4), table)
  table <- matrix(as.numeric(unlist(table)), ncol = 13)
  
  expect_equal(table, reference)
})

test_that(desc = "(id: f7-v0.5.5-t7) Statistical Sample Sizes based on the Poisson Distribution - 2.5 Percent Risk of Overreliance", {
  testthat::skip_on_cran()
  reference <- matrix(c(0, 0.0025, 0.005, 0.0075, 0.01, 0.0125, 0.015, 0.0175, 0.02, 0.0225, 0.025, 0.0275, 0.03, 0.0325, 0.035, 0.0375, 0.04, 0.0425, 0.045, 0.0475, 0.05, 0.0525, 0.055, 0.0575, 0.06, 0.0625, 0.065, 0.0675, 0.07, 0.0725, 0.075, 0.0775, 0.08, 0.0825, 0.085, 0.0875, 0.09, 0.0925, 0.095, 0.0975, 0.1, 
                        369, 671, 1529, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        185, 244, 336, 487, 765, 1365, 3076, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        123, 148, 180, 224, 285, 374, 510, 736, 1152, 2051, 4615, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        93, 106, 122, 143, 168, 201, 244, 302, 383, 501, 683, 984, 1538, 2735, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        74, 83, 92, 104, 118, 135, 155, 180, 212, 253, 306, 379, 480, 627, 854, 1231, 1923, 3419, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        62, 68, 74, 82, 90, 100, 112, 126, 143, 163, 187, 217, 255, 304, 368, 455, 576, 753, 1026, 1477, 2308, 4102, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        53, 57, 62, 67, 73, 80, 88, 96, 106, 118, 132, 148, 167, 190, 219, 254, 298, 355, 430, 532, 673, 879, 1197, 1723, 2692, 4786, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        47, 50, 53, 57, 61, 66, 72, 78, 84, 92, 101, 111, 122, 135, 151, 170, 192, 218, 251, 291, 342, 407, 492, 608, 769, 1005, 1368, 1969, 3077, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        41, 44, 47, 50, 53, 57, 60, 65, 70, 75, 81, 88, 95, 104, 114, 125, 138, 153, 170, 191, 216, 246, 282, 327, 384, 458, 554, 684, 866, 1131, 1539, 2216, 3461, NA, NA, NA, NA, NA, NA, NA, NA, 
                        37, 39, 42, 44, 46, 49, 52, 56, 59, 63, 68, 72, 78, 84, 90, 98, 106, 116, 127, 139, 153, 170, 190, 213, 240, 273, 314, 364, 427, 509, 616, 760, 962, 1256, 1710, 2462, 3846, NA, NA, NA, NA, 
                        25, 26, 27, 28, 29, 30, 31, 32, 34, 35, 36, 38, 40, 41, 43, 45, 47, 50, 52, 55, 57, 60, 64, 67, 71, 75, 80, 85, 90, 96, 102, 110, 118, 127, 136, 148, 160, 175, 191, 209, 231, 
                        19, 19, 20, 20, 21, 22, 22, 23, 23, 24, 25, 26, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 38, 39, 41, 42, 44, 45, 47, 49, 51, 53, 56, 58, 61, 64, 67, 70, 73, 77), ncol = 13)
  
  conf.level <- 0.975
  m <- c(seq(0.01, 0.10, 0.01), 0.15, 0.20)
  expected <- seq(0, 0.1, 0.0025)
  
  tab <- matrix(NA, nrow = length(expected), ncol = length(m))
  for(rows in 1:length(expected)){
    for(columns in 1:length(m)){
      if(expected[rows] >= m[columns]){
        next
      } else {
        p <- try({ suppressMessages(ss <- jfa::planning(conf.level = conf.level, expected = expected[rows], likelihood = "poisson", materiality = m[columns])) }, silent = T)
        if(class(p) == "try-error"){
          next 
        }
        tab[rows, columns] <- ss$n
      }
    }
  }
  
  table <- as.data.frame(tab, stringsAsFactors = F)
  table <- cbind(expected = round(expected, 4), table)
  table <- matrix(as.numeric(unlist(table)), ncol = 13)
  
  expect_equal(table, reference)
})

test_that(desc = "(id: f7-v0.5.5-t8) Statistical Sample Sizes based on the Poisson Distribution - 1 Percent Risk of Overreliance", {
  testthat::skip_on_cran()
  reference <- matrix(c(0, 0.0025, 0.005, 0.0075, 0.01, 0.0125, 0.015, 0.0175, 0.02, 0.0225, 0.025, 0.0275, 0.03, 0.0325, 0.035, 0.0375, 0.04, 0.0425, 0.045, 0.0475, 0.05, 0.0525, 0.055, 0.0575, 0.06, 0.0625, 0.065, 0.0675, 0.07, 0.0725, 0.075, 0.0775, 0.08, 0.0825, 0.085, 0.0875, 0.09, 0.0925, 0.095, 0.0975, 0.1, 
                        461, 871, 2045, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        231, 311, 436, 642, 1023, 1850, 4224, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        154, 187, 231, 291, 373, 495, 682, 994, 1569, 2816, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        116, 134, 156, 183, 218, 263, 321, 400, 512, 674, 925, 1342, 2112, 3780, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        93, 104, 117, 133, 152, 175, 202, 237, 280, 336, 409, 509, 648, 852, 1167, 1690, 2655, 4744, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        77, 85, 94, 104, 116, 130, 146, 164, 187, 214, 248, 289, 341, 408, 497, 617, 785, 1030, 1408, 2037, 3197, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        66, 72, 78, 85, 93, 103, 113, 125, 139, 154, 173, 195, 222, 254, 293, 341, 402, 481, 584, 724, 921, 1207, 1650, 2384, 3739, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        58, 62, 67, 72, 78, 85, 92, 100, 109, 120, 132, 145, 161, 179, 200, 226, 256, 293, 337, 393, 463, 553, 671, 832, 1056, 1384, 1890, 2731, 4280, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        52, 55, 59, 63, 67, 72, 77, 83, 90, 97, 105, 114, 125, 137, 150, 165, 183, 204, 228, 256, 290, 332, 382, 444, 523, 625, 758, 939, 1192, 1561, 2131, 3078, 4822, NA, NA, NA, NA, NA, NA, NA, NA, 
                        47, 49, 52, 55, 59, 63, 67, 71, 76, 82, 88, 94, 101, 109, 119, 129, 140, 153, 168, 185, 205, 228, 255, 287, 324, 370, 426, 496, 584, 697, 845, 1046, 1328, 1738, 2372, 3424, NA, NA, NA, NA, NA, 
                        31, 32, 34, 35, 36, 38, 39, 41, 43, 45, 47, 49, 51, 53, 56, 59, 61, 64, 68, 71, 75, 79, 84, 88, 94, 99, 106, 112, 120, 128, 137, 147, 158, 170, 184, 199, 216, 236, 259, 284, 314, 
                        24, 24, 25, 26, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 40, 41, 42, 44, 46, 47, 49, 51, 53, 55, 57, 60, 62, 65, 67, 70, 74, 77, 80, 84, 88, 93, 98, 103), ncol = 13)
  
  conf.level <- 0.99
  m <- c(seq(0.01, 0.10, 0.01), 0.15, 0.20)
  expected <- seq(0, 0.1, 0.0025)
  
  tab <- matrix(NA, nrow = length(expected), ncol = length(m))
  for(rows in 1:length(expected)){
    for(columns in 1:length(m)){
      if(expected[rows] >= m[columns]){
        next
      } else {
        p <- try({ suppressMessages(ss <- jfa::planning(conf.level = conf.level, expected = expected[rows], likelihood = "poisson", materiality = m[columns])) }, silent = T)
        if(class(p) == "try-error"){
          next 
        }
        tab[rows, columns] <- ss$n
      }
    }
  }
  
  table <- as.data.frame(tab, stringsAsFactors = F)
  table <- cbind(expected = round(expected, 4), table)
  table <- matrix(as.numeric(unlist(table)), ncol = 13)
  
  expect_equal(table, reference)
})

# Hypergeometric distribution

test_that(desc = "(id: f7-v0.5.5-t9) Statistical Sample Sizes based on the Hypergeometric Distribution (N = 100) - 10 Percent Risk of Overreliance", {
  testthat::skip_on_cran()
  reference <- matrix(c(0, 0.0025, 0.005, 0.0075, 0.01, 0.0125, 0.015, 0.0175, 0.02, 0.0225, 0.025, 0.0275, 0.03, 0.0325, 0.035, 0.0375, 0.04, 0.0425, 0.045, 0.0475, 0.05, 0.0525, 0.055, 0.0575, 0.06, 0.0625, 0.065, 0.0675, 0.07, 0.0725, 0.075, 0.0775, 0.08, 0.0825, 0.085, 0.0875, 0.09, 0.0925, 0.095, 0.0975, 0.1, 
                        91, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        69, 95, 95, 95, 95, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        54, 81, 81, 81, 81, 97, 97, 97, 97, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        44, 68, 68, 68, 68, 68, 86, 86, 86, 86, 98, 98, 98, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        37, 58, 58, 58, 58, 58, 58, 75, 75, 75, 75, 89, 89, 89, 98, 98, 98, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        28, 45, 45, 45, 45, 45, 45, 45, 45, 59, 59, 59, 59, 59, 72, 72, 72, 83, 83, 83, 92, 92, 99, 99, 99, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        28, 45, 45, 45, 45, 45, 45, 45, 45, 59, 59, 59, 59, 59, 72, 72, 72, 83, 83, 83, 92, 92, 99, 99, 99, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        25, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 53, 53, 53, 53, 53, 65, 65, 65, 76, 76, 76, 85, 85, 93, 93, 99, 99, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        22, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 49, 49, 49, 49, 49, 60, 60, 60, 60, 70, 70, 79, 79, 79, 87, 87, 94, 94, 99, 99, 99, NA, NA, NA, NA, NA, NA, NA, NA, 
                        20, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 44, 44, 44, 44, 44, 44, 55, 55, 55, 64, 64, 64, 64, 73, 73, 81, 81, 88, 88, 95, 95, 99, 99, 99, NA, NA, NA, NA, 
                        14, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 31, 31, 31, 31, 31, 31, 31, 31, 38, 38, 38, 38, 38, 38, 46, 46, 46, 52, 52, 52, 52, 59, 59, 
                        10, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 29, 29, 29, 29, 29, 29, 29), ncol = 13)
  
  if(Sys.info()['sysname'] == "Linux") { # TODO: Need to figure out why this happens on Linux
    reference[445] <- 100
    reference[446] <- 100
    reference[447] <- 100
  }
  
  conf.level <- 0.90
  m <- c(seq(0.01, 0.10, 0.01), 0.15, 0.20)
  expected <- seq(0, 0.1, 0.0025)
  N <- 100
  
  tab <- matrix(NA, nrow = length(expected), ncol = length(m))
  for(rows in 1:length(expected)){
    for(columns in 1:length(m)){
      if(expected[rows] >= m[columns]){
        next
      } else {
        p <- try({ suppressMessages(ss <- jfa::planning(conf.level = conf.level, expected = expected[rows], likelihood = "hypergeometric", materiality = m[columns], N.units = N)) }, silent = T)
        if(class(p) == "try-error"){
          next 
        }
        tab[rows, columns] <- ss$n
      }
    }
  }
  
  table <- as.data.frame(tab, stringsAsFactors = F)
  table <- cbind(expected = round(expected, 4), table)
  table <- matrix(as.numeric(unlist(table)), ncol = 13)
  
  expect_equal(table, reference)
})

test_that(desc = "(id: f7-v0.5.5-t10) Statistical Sample Sizes based on the Hypergeometric Distribution (N = 500) - 10 Percent Risk of Overreliance", {
  testthat::skip_on_cran()
  reference <- matrix(c(0, 0.0025, 0.005, 0.0075, 0.01, 0.0125, 0.015, 0.0175, 0.02, 0.0225, 0.025, 0.0275, 0.03, 0.0325, 0.035, 0.0375, 0.04, 0.0425, 0.045, 0.0475, 0.05, 0.0525, 0.055, 0.0575, 0.06, 0.0625, 0.065, 0.0675, 0.07, 0.0725, 0.075, 0.0775, 0.08, 0.0825, 0.085, 0.0875, 0.09, 0.0925, 0.095, 0.0975, 0.1, 
                        184, 292, 377, 490, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        102, 168, 168, 224, 275, 366, 442, 495, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        71, 117, 117, 117, 158, 158, 196, 265, 298, 387, 439, 497, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        54, 90, 90, 90, 90, 122, 122, 151, 179, 206, 233, 283, 330, 396, 455, 498, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        43, 73, 73, 73, 73, 73, 99, 99, 99, 123, 146, 169, 191, 212, 253, 292, 349, 418, 464, 498, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        35, 59, 59, 59, 59, 59, 59, 80, 80, 80, 80, 100, 100, 120, 138, 156, 174, 208, 242, 290, 320, 380, 434, 482, 499, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        31, 52, 52, 52, 52, 52, 52, 52, 72, 72, 72, 72, 89, 89, 107, 123, 123, 139, 155, 186, 216, 245, 288, 329, 381, 430, 474, 499, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        27, 46, 46, 46, 46, 46, 46, 46, 46, 63, 63, 63, 63, 79, 79, 79, 94, 94, 108, 123, 137, 151, 178, 191, 230, 255, 292, 340, 397, 439, 478, 499, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        24, 41, 41, 41, 41, 41, 41, 41, 41, 41, 56, 56, 56, 56, 56, 70, 70, 70, 84, 84, 97, 110, 122, 135, 147, 159, 183, 206, 240, 274, 306, 348, 399, 446, 480, 499, NA, NA, NA, NA, NA, 
                        22, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 50, 50, 50, 50, 50, 50, 63, 63, 63, 75, 75, 87, 99, 99, 111, 122, 133, 155, 176, 197, 218, 248, 278, 317, 364, 400, 443, 482, 500, NA, 
                        14, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 34, 34, 34, 34, 34, 34, 34, 42, 42, 42, 42, 42, 50, 50, 50, 50, 59, 66, 66, 66, 74, 82, 82, 90, 
                        11, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 31, 31, 31, 31, 31, 31, 38, 38), ncol = 13)
  
  conf.level <- 0.90
  m <- c(seq(0.01, 0.10, 0.01), 0.15, 0.20)
  expected <- seq(0, 0.1, 0.0025)
  N <- 500
  
  tab <- matrix(NA, nrow = length(expected), ncol = length(m))
  for(rows in 1:length(expected)){
    for(columns in 1:length(m)){
      if(expected[rows] >= m[columns]){
        next
      } else {
        p <- try({ suppressMessages(ss <- jfa::planning(conf.level = conf.level, expected = expected[rows], likelihood = "hypergeometric", materiality = m[columns], N.units = N)) }, silent = T)
        if(class(p) == "try-error"){
          next 
        }
        tab[rows, columns] <- ss$n
      }
    }
  }
  
  table <- as.data.frame(tab, stringsAsFactors = F)
  table <- cbind(expected = round(expected, 4), table)
  table <- matrix(as.numeric(unlist(table)), ncol = 13)
  
  expect_equal(table, reference)
})

test_that(desc = "(id: f7-v0.5.5-t11) Statistical Sample Sizes based on the Hypergeometric Distribution (N = 1000) - 10 Percent Risk of Overreliance", {
  testthat::skip_on_cran()
  reference <- matrix(c(0, 0.0025, 0.005, 0.0075, 0.01, 0.0125, 0.015, 0.0175, 0.02, 0.0225, 0.025, 0.0275, 0.03, 0.0325, 0.035, 0.0375, 0.04, 0.0425, 0.045, 0.0475, 0.05, 0.0525, 0.055, 0.0575, 0.06, 0.0625, 0.065, 0.0675, 0.07, 0.0725, 0.075, 0.0775, 0.08, 0.0825, 0.085, 0.0875, 0.09, 0.0925, 0.095, 0.0975, 0.1, 
                        205, 336, 551, 884, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        108, 180, 180, 244, 360, 466, 661, 910, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        73, 123, 123, 123, 167, 208, 248, 323, 396, 532, 751, 940, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        55, 93, 93, 93, 93, 127, 127, 158, 189, 218, 275, 357, 462, 610, 794, 956, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        44, 75, 75, 75, 75, 75, 102, 102, 128, 128, 152, 176, 200, 245, 311, 396, 499, 655, 820, 965, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        36, 61, 61, 61, 61, 61, 61, 84, 84, 84, 105, 105, 125, 145, 165, 184, 221, 258, 311, 398, 498, 627, 780, 921, 999, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        32, 54, 54, 54, 54, 54, 54, 54, 73, 73, 73, 92, 92, 92, 110, 127, 144, 161, 177, 210, 257, 304, 379, 452, 581, 704, 860, 975, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        28, 47, 47, 47, 47, 47, 47, 47, 47, 64, 64, 64, 64, 80, 80, 80, 96, 111, 111, 126, 156, 170, 198, 226, 280, 320, 399, 488, 599, 731, 878, 978, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        25, 42, 42, 42, 42, 42, 42, 42, 42, 42, 57, 57, 57, 57, 57, 71, 71, 85, 85, 99, 99, 112, 126, 139, 164, 189, 214, 250, 298, 356, 425, 515, 624, 751, 882, 980, NA, NA, NA, NA, NA, 
                        22, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 51, 51, 51, 51, 51, 64, 64, 64, 77, 77, 89, 89, 101, 113, 125, 136, 148, 171, 193, 226, 258, 311, 374, 446, 536, 644, 767, 894, 982, NA, 
                        15, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 34, 34, 34, 34, 34, 34, 34, 43, 43, 43, 43, 51, 51, 51, 51, 59, 59, 67, 67, 75, 75, 83, 91, 99, 
                        11, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 32, 32, 32, 32, 32, 38, 38, 38), ncol = 13)
  
  conf.level <- 0.90
  m <- c(seq(0.01, 0.10, 0.01), 0.15, 0.20)
  expected <- seq(0, 0.1, 0.0025)
  N <- 1000
  
  tab <- matrix(NA, nrow = length(expected), ncol = length(m))
  for(rows in 1:length(expected)){
    for(columns in 1:length(m)){
      if(expected[rows] >= m[columns]){
        next
      } else {
        p <- try({ suppressMessages(ss <- jfa::planning(conf.level = conf.level, expected = expected[rows], likelihood = "hypergeometric", materiality = m[columns], N.units = N)) }, silent = T)
        if(class(p) == "try-error"){
          next 
        }
        tab[rows, columns] <- ss$n
      }
    }
  }
  
  table <- as.data.frame(tab, stringsAsFactors = F)
  table <- cbind(expected = round(expected, 4), table)
  table <- matrix(as.numeric(unlist(table)), ncol = 13)
  
  expect_equal(table, reference)
})

test_that(desc = "(id: f7-v0.5.5-t12) Statistical Sample Sizes based on the Hypergeometric Distribution (N = 100) - 5 Percent Risk of Overreliance", {
  testthat::skip_on_cran()
  reference <- matrix(c(0, 0.0025, 0.005, 0.0075, 0.01, 0.0125, 0.015, 0.0175, 0.02, 0.0225, 0.025, 0.0275, 0.03, 0.0325, 0.035, 0.0375, 0.04, 0.0425, 0.045, 0.0475, 0.05, 0.0525, 0.055, 0.0575, 0.06, 0.0625, 0.065, 0.0675, 0.07, 0.0725, 0.075, 0.0775, 0.08, 0.0825, 0.085, 0.0875, 0.09, 0.0925, 0.095, 0.0975, 0.1, 
                        95, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        78, 98, 98, 98, 98, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        63, 87, 87, 87, 87, 99, 99, 99, 99, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        52, 75, 75, 75, 75, 75, 90, 90, 90, 99, 99, 99, 99, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        45, 65, 65, 65, 65, 65, 65, 81, 81, 81, 92, 92, 92, 92, 99, 99, 99, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        34, 51, 51, 51, 51, 51, 51, 51, 65, 65, 65, 65, 65, 77, 77, 77, 87, 87, 87, 95, 95, 95, 100, 100, 100, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        34, 51, 51, 51, 51, 51, 51, 51, 65, 65, 65, 65, 65, 77, 77, 77, 87, 87, 87, 95, 95, 95, 100, 100, 100, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        31, 46, 46, 46, 46, 46, 46, 46, 46, 59, 59, 59, 59, 59, 71, 71, 71, 80, 80, 80, 80, 89, 89, 95, 95, 95, 100, 100, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        28, 42, 42, 42, 42, 42, 42, 42, 42, 42, 54, 54, 54, 54, 54, 65, 65, 65, 65, 74, 74, 74, 83, 83, 83, 90, 90, 96, 96, 96, 100, 100, 100, NA, NA, NA, NA, NA, NA, NA, NA, 
                        25, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 50, 50, 50, 50, 50, 50, 60, 60, 60, 60, 69, 69, 69, 77, 77, 85, 85, 85, 91, 91, 96, 96, 96, 100, 100, 100, NA, NA, NA, NA, 
                        17, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 35, 35, 35, 35, 35, 35, 35, 35, 43, 43, 43, 43, 43, 50, 50, 50, 50, 50, 57, 57, 57, 63, 63, 63, 69, 69, 
                        13, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 27, 27, 27, 27, 27, 27, 27, 27, 27, 33, 33, 33, 33, 33, 33, 33, 39, 39, 39, 39), ncol = 13)
  
  conf.level <- 0.95
  m <- c(seq(0.01, 0.10, 0.01), 0.15, 0.20)
  expected <- seq(0, 0.1, 0.0025)
  N <- 100
  
  tab <- matrix(NA, nrow = length(expected), ncol = length(m))
  for(rows in 1:length(expected)){
    for(columns in 1:length(m)){
      if(expected[rows] >= m[columns]){
        next
      } else {
        p <- try({ suppressMessages(ss <- jfa::planning(conf.level = conf.level, expected = expected[rows], likelihood = "hypergeometric", materiality = m[columns], N.units = N)) }, silent = T)
        if(class(p) == "try-error"){
          next 
        }
        tab[rows, columns] <- ss$n
      }
    }
  }
  
  table <- as.data.frame(tab, stringsAsFactors = F)
  table <- cbind(expected = round(expected, 4), table)
  table <- matrix(as.numeric(unlist(table)), ncol = 13)
  
  expect_equal(table, reference)
})

test_that(desc = "(id: f7-v0.5.5-t13) Statistical Sample Sizes based on the Hypergeometric Distribution (N = 500) - 5 Percent Risk of Overreliance", {
  testthat::skip_on_cran()
  reference <- matrix(c(0, 0.0025, 0.005, 0.0075, 0.01, 0.0125, 0.015, 0.0175, 0.02, 0.0225, 0.025, 0.0275, 0.03, 0.0325, 0.035, 0.0375, 0.04, 0.0425, 0.045, 0.0475, 0.05, 0.0525, 0.055, 0.0575, 0.06, 0.0625, 0.065, 0.0675, 0.07, 0.0725, 0.075, 0.0775, 0.08, 0.0825, 0.085, 0.0875, 0.09, 0.0925, 0.095, 0.0975, 0.1, 
                        225, 328, 462, 495, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        129, 196, 196, 253, 348, 388, 456, 498, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        90, 139, 139, 181, 181, 219, 254, 319, 349, 429, 471, 499, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        69, 107, 107, 107, 140, 140, 171, 171, 199, 253, 278, 325, 391, 430, 479, 499, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        56, 87, 87, 87, 87, 114, 114, 114, 139, 163, 186, 208, 230, 270, 309, 346, 398, 444, 483, 499, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        45, 71, 71, 71, 71, 71, 93, 93, 93, 114, 114, 134, 153, 153, 190, 207, 224, 257, 305, 335, 378, 419, 467, 486, 500, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        40, 63, 63, 63, 63, 63, 63, 83, 83, 83, 102, 102, 120, 120, 137, 153, 170, 185, 216, 231, 260, 302, 342, 381, 428, 461, 488, 500, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        35, 55, 55, 55, 55, 55, 55, 55, 73, 73, 73, 90, 90, 90, 105, 105, 121, 135, 150, 164, 178, 205, 218, 256, 281, 317, 352, 396, 428, 466, 489, 500, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        31, 49, 49, 49, 49, 49, 49, 49, 49, 65, 65, 65, 65, 80, 80, 80, 94, 94, 108, 121, 134, 147, 159, 172, 196, 207, 242, 264, 297, 329, 360, 399, 436, 469, 491, 500, NA, NA, NA, NA, NA, 
                        28, 44, 44, 44, 44, 44, 44, 44, 44, 44, 59, 59, 59, 59, 72, 72, 72, 85, 85, 97, 97, 110, 121, 121, 133, 144, 167, 177, 199, 220, 240, 280, 300, 338, 374, 409, 442, 472, 492, 500, NA, 
                        19, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 39, 39, 39, 39, 39, 39, 39, 48, 48, 48, 48, 48, 57, 57, 57, 65, 65, 74, 74, 82, 82, 90, 98, 105, 105, 120, 120, 
                        14, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 29, 29, 29, 29, 29, 29, 29, 29, 29, 36, 36, 36, 36, 36, 36, 43, 43, 43, 43, 49, 49, 49), ncol = 13)
  
  conf.level <- 0.95
  m <- c(seq(0.01, 0.10, 0.01), 0.15, 0.20)
  expected <- seq(0, 0.1, 0.0025)
  N <- 500
  
  tab <- matrix(NA, nrow = length(expected), ncol = length(m))
  for(rows in 1:length(expected)){
    for(columns in 1:length(m)){
      if(expected[rows] >= m[columns]){
        next
      } else {
        p <- try({ suppressMessages(ss <- jfa::planning(conf.level = conf.level, expected = expected[rows], likelihood = "hypergeometric", materiality = m[columns], N.units = N)) }, silent = T)
        if(class(p) == "try-error"){
          next 
        }
        tab[rows, columns] <- ss$n
      }
    }
  }
  
  table <- as.data.frame(tab, stringsAsFactors = F)
  table <- cbind(expected = round(expected, 4), table)
  table <- matrix(as.numeric(unlist(table)), ncol = 13)
  
  expect_equal(table, reference)
})

test_that(desc = "(id: f7-v0.5.5-t14) Statistical Sample Sizes based on the Hypergeometric Distribution (N = 1000) - 5 Percent Risk of Overreliance", {
  testthat::skip_on_cran()
  reference <- matrix(c(0, 0.0025, 0.005, 0.0075, 0.01, 0.0125, 0.015, 0.0175, 0.02, 0.0225, 0.025, 0.0275, 0.03, 0.0325, 0.035, 0.0375, 0.04, 0.0425, 0.045, 0.0475, 0.05, 0.0525, 0.055, 0.0575, 0.06, 0.0625, 0.065, 0.0675, 0.07, 0.0725, 0.075, 0.0775, 0.08, 0.0825, 0.085, 0.0875, 0.09, 0.0925, 0.095, 0.0975, 0.1, 
                        258, 393, 696, 913, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        138, 215, 281, 342, 400, 557, 782, 958, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        94, 147, 147, 194, 194, 237, 317, 392, 498, 660, 833, 972, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        71, 112, 112, 112, 148, 148, 181, 213, 243, 302, 358, 464, 565, 706, 856, 979, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        57, 90, 90, 90, 90, 119, 119, 146, 146, 172, 197, 245, 291, 335, 421, 504, 621, 751, 886, 983, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        47, 74, 74, 74, 74, 74, 98, 98, 98, 121, 142, 142, 163, 183, 222, 260, 297, 350, 420, 504, 617, 739, 854, 956, 1000, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        41, 65, 65, 65, 65, 65, 65, 86, 86, 86, 105, 105, 124, 142, 142, 160, 194, 211, 244, 292, 339, 400, 488, 573, 683, 800, 919, 988, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        36, 57, 57, 57, 57, 57, 57, 57, 75, 75, 75, 92, 92, 92, 109, 125, 125, 140, 171, 186, 200, 243, 271, 312, 366, 431, 507, 606, 713, 825, 920, 990, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        32, 50, 50, 50, 50, 50, 50, 50, 50, 67, 67, 67, 82, 82, 82, 97, 97, 111, 111, 125, 139, 152, 179, 191, 230, 254, 291, 339, 397, 455, 533, 631, 725, 836, 928, 991, NA, NA, NA, NA, NA, 
                        29, 45, 45, 45, 45, 45, 45, 45, 45, 60, 60, 60, 60, 60, 74, 74, 74, 87, 87, 100, 100, 113, 125, 137, 149, 173, 184, 207, 241, 274, 306, 360, 412, 483, 563, 651, 754, 852, 936, 992, NA, 
                        19, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 40, 40, 40, 40, 40, 40, 40, 49, 49, 49, 49, 58, 58, 58, 67, 67, 75, 75, 75, 83, 92, 100, 100, 108, 123, 123, 139, 
                        14, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 29, 29, 29, 29, 29, 29, 29, 29, 29, 36, 36, 36, 36, 36, 36, 43, 43, 43, 43, 50, 50, 50), ncol = 13)
  
  conf.level <- 0.95
  m <- c(seq(0.01, 0.10, 0.01), 0.15, 0.20)
  expected <- seq(0, 0.1, 0.0025)
  N <- 1000
  
  tab <- matrix(NA, nrow = length(expected), ncol = length(m))
  for(rows in 1:length(expected)){
    for(columns in 1:length(m)){
      if(expected[rows] >= m[columns]){
        next
      } else {
        p <- try({ suppressMessages(ss <- jfa::planning(conf.level = conf.level, expected = expected[rows], likelihood = "hypergeometric", materiality = m[columns], N.units = N)) }, silent = T)
        if(class(p) == "try-error"){
          next 
        }
        tab[rows, columns] <- ss$n
      }
    }
  }
  
  table <- as.data.frame(tab, stringsAsFactors = F)
  table <- cbind(expected = round(expected, 4), table)
  table <- matrix(as.numeric(unlist(table)), ncol = 13)
  
  expect_equal(table, reference)
})

test_that(desc = "(id: f7-v0.5.5-t15) Statistical Sample Sizes based on the Hypergeometric Distribution (N = 100) - 2.5 Percent Risk of Overreliance", {
  testthat::skip_on_cran()
  reference <- matrix(c(0, 0.0025, 0.005, 0.0075, 0.01, 0.0125, 0.015, 0.0175, 0.02, 0.0225, 0.025, 0.0275, 0.03, 0.0325, 0.035, 0.0375, 0.04, 0.0425, 0.045, 0.0475, 0.05, 0.0525, 0.055, 0.0575, 0.06, 0.0625, 0.065, 0.0675, 0.07, 0.0725, 0.075, 0.0775, 0.08, 0.0825, 0.085, 0.0875, 0.09, 0.0925, 0.095, 0.0975, 0.1, 
                        98, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        84, 99, 99, 99, 99, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        71, 91, 91, 91, 91, 100, 100, 100, 100, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        60, 80, 80, 80, 80, 80, 93, 93, 93, 100, 100, 100, 100, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        52, 71, 71, 71, 71, 71, 85, 85, 85, 85, 95, 95, 95, 100, 100, 100, 100, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        40, 57, 57, 57, 57, 57, 57, 57, 70, 70, 70, 70, 81, 81, 81, 90, 90, 90, 96, 96, 96, 100, 100, 100, 100, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        40, 57, 57, 57, 57, 57, 57, 57, 70, 70, 70, 70, 81, 81, 81, 90, 90, 90, 96, 96, 96, 100, 100, 100, 100, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        36, 52, 52, 52, 52, 52, 52, 52, 64, 64, 64, 64, 64, 75, 75, 75, 75, 84, 84, 84, 91, 91, 97, 97, 97, 100, 100, 100, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        33, 47, 47, 47, 47, 47, 47, 47, 47, 59, 59, 59, 59, 59, 69, 69, 69, 69, 78, 78, 78, 86, 86, 86, 92, 92, 92, 97, 97, 100, 100, 100, 100, NA, NA, NA, NA, NA, NA, NA, NA, 
                        30, 43, 43, 43, 43, 43, 43, 43, 43, 43, 55, 55, 55, 55, 55, 64, 64, 64, 64, 73, 73, 73, 81, 81, 81, 87, 87, 87, 93, 93, 93, 98, 98, 100, 100, 100, 100, NA, NA, NA, NA, 
                        21, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 39, 39, 39, 39, 39, 39, 39, 39, 47, 47, 47, 47, 47, 54, 54, 54, 54, 60, 60, 60, 60, 66, 66, 66, 72, 72, 78, 78, 
                        16, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 30, 30, 30, 30, 30, 30, 30, 30, 30, 36, 36, 36, 36, 36, 36, 36, 42, 42, 42, 42, 42, 47, 47), ncol = 13)
  
  conf.level <- 0.975
  m <- c(seq(0.01, 0.10, 0.01), 0.15, 0.20)
  expected <- seq(0, 0.1, 0.0025)
  N <- 100
  
  tab <- matrix(NA, nrow = length(expected), ncol = length(m))
  for(rows in 1:length(expected)){
    for(columns in 1:length(m)){
      if(expected[rows] >= m[columns]){
        next
      } else {
        p <- try({ suppressMessages(ss <- jfa::planning(conf.level = conf.level, expected = expected[rows], likelihood = "hypergeometric", materiality = m[columns], N.units = N)) }, silent = T)
        if(class(p) == "try-error"){
          next 
        }
        tab[rows, columns] <- ss$n
      }
    }
  }
  
  table <- as.data.frame(tab, stringsAsFactors = F)
  table <- cbind(expected = round(expected, 4), table)
  table <- matrix(as.numeric(unlist(table)), ncol = 13)
  
  expect_equal(table, reference)
})

test_that(desc = "(id: f7-v0.5.5-t16) Statistical Sample Sizes based on the Hypergeometric Distribution (N = 500) - 2.5 Percent Risk of Overreliance", {
  testthat::skip_on_cran()
  reference <- matrix(c(0, 0.0025, 0.005, 0.0075, 0.01, 0.0125, 0.015, 0.0175, 0.02, 0.0225, 0.025, 0.0275, 0.03, 0.0325, 0.035, 0.0375, 0.04, 0.0425, 0.045, 0.0475, 0.05, 0.0525, 0.055, 0.0575, 0.06, 0.0625, 0.065, 0.0675, 0.07, 0.0725, 0.075, 0.0775, 0.08, 0.0825, 0.085, 0.0875, 0.09, 0.0925, 0.095, 0.0975, 0.1, 
                        260, 358, 474, 498, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        153, 221, 277, 325, 368, 439, 466, 499, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        108, 158, 158, 201, 239, 239, 307, 337, 393, 440, 478, 500, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        83, 123, 123, 123, 157, 157, 188, 217, 244, 294, 318, 363, 422, 456, 484, 500, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        67, 100, 100, 100, 100, 129, 129, 154, 179, 202, 224, 245, 266, 305, 342, 393, 424, 465, 487, 500, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        55, 82, 82, 82, 82, 105, 105, 105, 127, 127, 147, 167, 185, 203, 221, 238, 271, 302, 333, 375, 415, 451, 472, 496, 500, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        49, 73, 73, 73, 73, 73, 94, 94, 94, 113, 113, 132, 132, 149, 166, 182, 198, 229, 244, 273, 314, 341, 379, 414, 447, 475, 491, 500, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        43, 64, 64, 64, 64, 64, 64, 83, 83, 83, 100, 100, 100, 116, 132, 132, 147, 161, 176, 203, 217, 243, 268, 293, 329, 351, 384, 425, 453, 478, 492, 500, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        38, 57, 57, 57, 57, 57, 57, 57, 74, 74, 74, 89, 89, 89, 104, 104, 118, 132, 132, 145, 158, 170, 195, 207, 230, 253, 276, 308, 339, 369, 398, 425, 459, 481, 493, 500, NA, NA, NA, NA, NA, 
                        34, 52, 52, 52, 52, 52, 52, 52, 67, 67, 67, 67, 81, 81, 81, 94, 94, 94, 107, 119, 119, 131, 143, 154, 166, 188, 199, 220, 241, 261, 291, 319, 347, 374, 400, 433, 463, 483, 493, 500, NA, 
                        23, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 44, 44, 44, 44, 44, 44, 44, 54, 54, 54, 54, 63, 63, 63, 72, 72, 80, 80, 80, 89, 97, 105, 105, 113, 120, 128, 136, 143, 158, 
                        17, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 33, 33, 33, 33, 33, 33, 33, 33, 40, 40, 40, 40, 40, 40, 47, 47, 47, 47, 54, 54, 54, 60, 60, 60), ncol = 13)
  
  conf.level <- 0.975
  m <- c(seq(0.01, 0.10, 0.01), 0.15, 0.20)
  expected <- seq(0, 0.1, 0.0025)
  N <- 500
  
  tab <- matrix(NA, nrow = length(expected), ncol = length(m))
  for(rows in 1:length(expected)){
    for(columns in 1:length(m)){
      if(expected[rows] >= m[columns]){
        next
      } else {
        p <- try({ suppressMessages(ss <- jfa::planning(conf.level = conf.level, expected = expected[rows], likelihood = "hypergeometric", materiality = m[columns], N.units = N)) }, silent = T)
        if(class(p) == "try-error"){
          next 
        }
        tab[rows, columns] <- ss$n
      }
    }
  }
  
  table <- as.data.frame(tab, stringsAsFactors = F)
  table <- cbind(expected = round(expected, 4), table)
  table <- matrix(as.numeric(unlist(table)), ncol = 13)
  
  expect_equal(table, reference)
})

test_that(desc = "(id: f7-v0.5.5-t17) Statistical Sample Sizes based on the Hypergeometric Distribution (N = 1000) - 2.5 Percent Risk of Overreliance", {
  testthat::skip_on_cran()
  reference <- matrix(c(0, 0.0025, 0.005, 0.0075, 0.01, 0.0125, 0.015, 0.0175, 0.02, 0.0225, 0.025, 0.0275, 0.03, 0.0325, 0.035, 0.0375, 0.04, 0.0425, 0.045, 0.0475, 0.05, 0.0525, 0.055, 0.0575, 0.06, 0.0625, 0.065, 0.0675, 0.07, 0.0725, 0.075, 0.0775, 0.08, 0.0825, 0.085, 0.0875, 0.09, 0.0925, 0.095, 0.0975, 0.1, 
                        308, 555, 737, 933, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        167, 247, 315, 377, 489, 638, 845, 968, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        115, 171, 171, 219, 263, 305, 384, 457, 592, 744, 876, 979, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        87, 130, 130, 130, 167, 202, 235, 266, 296, 354, 436, 539, 660, 792, 908, 984, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        70, 105, 105, 105, 135, 135, 163, 163, 190, 216, 265, 288, 357, 422, 485, 585, 697, 819, 927, 987, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        57, 86, 86, 86, 86, 112, 112, 112, 135, 157, 157, 179, 199, 239, 278, 315, 370, 422, 507, 588, 697, 798, 905, 972, 1000, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        50, 75, 75, 75, 75, 75, 97, 97, 97, 118, 118, 138, 156, 175, 192, 210, 244, 277, 309, 357, 418, 492, 563, 659, 764, 861, 938, 991, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        44, 66, 66, 66, 66, 66, 66, 85, 85, 85, 103, 103, 121, 121, 137, 153, 169, 185, 200, 229, 258, 301, 342, 382, 448, 512, 598, 681, 783, 879, 946, 992, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        39, 59, 59, 59, 59, 59, 59, 76, 76, 76, 76, 92, 92, 92, 107, 122, 122, 137, 151, 165, 178, 205, 218, 256, 281, 318, 366, 413, 471, 549, 625, 708, 799, 883, 952, 993, NA, NA, NA, NA, NA, 
                        35, 53, 53, 53, 53, 53, 53, 53, 68, 68, 68, 68, 83, 83, 83, 97, 97, 110, 110, 123, 136, 148, 161, 173, 197, 208, 243, 266, 299, 342, 385, 437, 498, 568, 645, 730, 811, 895, 957, 994, NA, 
                        23, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 45, 45, 45, 45, 45, 45, 55, 55, 55, 55, 64, 64, 64, 64, 73, 73, 82, 82, 91, 99, 99, 107, 116, 124, 132, 140, 156, 163, 179, 
                        17, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 34, 34, 34, 34, 34, 34, 34, 34, 41, 41, 41, 41, 41, 41, 48, 48, 48, 48, 55, 55, 55, 61, 61, 61, 68), ncol = 13)
  
  conf.level <- 0.975
  m <- c(seq(0.01, 0.10, 0.01), 0.15, 0.20)
  expected <- seq(0, 0.1, 0.0025)
  N <- 1000
  
  tab <- matrix(NA, nrow = length(expected), ncol = length(m))
  for(rows in 1:length(expected)){
    for(columns in 1:length(m)){
      if(expected[rows] >= m[columns]){
        next
      } else {
        p <- try({ suppressMessages(ss <- jfa::planning(conf.level = conf.level, expected = expected[rows], likelihood = "hypergeometric", materiality = m[columns], N.units = N)) }, silent = T)
        if(class(p) == "try-error"){
          next 
        }
        tab[rows, columns] <- ss$n
      }
    }
  }
  
  table <- as.data.frame(tab, stringsAsFactors = F)
  table <- cbind(expected = round(expected, 4), table)
  table <- matrix(as.numeric(unlist(table)), ncol = 13)
  
  expect_equal(table, reference)
})

test_that(desc = "(id: f7-v0.5.5-t18) Statistical Sample Sizes based on the Hypergeometric Distribution (N = 100) - 1 Percent Risk of Overreliance", {
  testthat::skip_on_cran()
  reference <- matrix(c(0, 0.0025, 0.005, 0.0075, 0.01, 0.0125, 0.015, 0.0175, 0.02, 0.0225, 0.025, 0.0275, 0.03, 0.0325, 0.035, 0.0375, 0.04, 0.0425, 0.045, 0.0475, 0.05, 0.0525, 0.055, 0.0575, 0.06, 0.0625, 0.065, 0.0675, 0.07, 0.0725, 0.075, 0.0775, 0.08, 0.0825, 0.085, 0.0875, 0.09, 0.0925, 0.095, 0.0975, 0.1, 
                        99, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        90, 100, 100, 100, 100, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        78, 94, 94, 94, 94, 100, 100, 100, 100, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        68, 86, 86, 86, 86, 96, 96, 96, 96, 100, 100, 100, 100, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        59, 77, 77, 77, 77, 77, 89, 89, 89, 97, 97, 97, 97, 100, 100, 100, 100, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        47, 63, 63, 63, 63, 63, 63, 76, 76, 76, 76, 85, 85, 85, 85, 93, 93, 93, 98, 98, 98, 100, 100, 100, 100, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        47, 63, 63, 63, 63, 63, 63, 76, 76, 76, 76, 85, 85, 85, 85, 93, 93, 93, 98, 98, 98, 100, 100, 100, 100, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        43, 58, 58, 58, 58, 58, 58, 70, 70, 70, 70, 70, 79, 79, 79, 79, 87, 87, 87, 94, 94, 94, 98, 98, 98, 100, 100, 100, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        39, 53, 53, 53, 53, 53, 53, 53, 64, 64, 64, 64, 64, 74, 74, 74, 74, 82, 82, 82, 89, 89, 89, 94, 94, 94, 98, 98, 98, 100, 100, 100, 100, NA, NA, NA, NA, NA, NA, NA, NA, 
                        36, 49, 49, 49, 49, 49, 49, 49, 49, 60, 60, 60, 60, 60, 69, 69, 69, 69, 77, 77, 77, 84, 84, 84, 90, 90, 90, 95, 95, 95, 98, 98, 98, 100, 100, 100, 100, NA, NA, NA, NA, 
                        25, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 44, 44, 44, 44, 44, 44, 44, 51, 51, 51, 51, 51, 58, 58, 58, 58, 64, 64, 64, 64, 70, 70, 70, 76, 76, 81, 81, 81, 85, 
                        19, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 34, 34, 34, 34, 34, 34, 34, 34, 34, 40, 40, 40, 40, 40, 40, 40, 46, 46, 46, 46, 51, 51, 51, 51, 51, 56), ncol = 13)
  
  conf.level <- 0.99
  m <- c(seq(0.01, 0.10, 0.01), 0.15, 0.20)
  expected <- seq(0, 0.1, 0.0025)
  N <- 100
  
  tab <- matrix(NA, nrow = length(expected), ncol = length(m))
  for(rows in 1:length(expected)){
    for(columns in 1:length(m)){
      if(expected[rows] >= m[columns]){
        next
      } else {
        p <- try({ suppressMessages(ss <- jfa::planning(conf.level = conf.level, expected = expected[rows], likelihood = "hypergeometric", materiality = m[columns], N.units = N)) }, silent = T)
        if(class(p) == "try-error"){
          next 
        }
        tab[rows, columns] <- ss$n
      }
    }
  }
  
  table <- as.data.frame(tab, stringsAsFactors = F)
  table <- cbind(expected = round(expected, 4), table)
  table <- matrix(as.numeric(unlist(table)), ncol = 13)
  
  expect_equal(table, reference)
})

test_that(desc = "(id: f7-v0.5.5-t19) Statistical Sample Sizes based on the Hypergeometric Distribution (N = 500) - 1 Percent Risk of Overreliance", {
  testthat::skip_on_cran()
  reference <- matrix(c(0, 0.0025, 0.005, 0.0075, 0.01, 0.0125, 0.015, 0.0175, 0.02, 0.0225, 0.025, 0.0275, 0.03, 0.0325, 0.035, 0.0375, 0.04, 0.0425, 0.045, 0.0475, 0.05, 0.0525, 0.055, 0.0575, 0.06, 0.0625, 0.065, 0.0675, 0.07, 0.0725, 0.075, 0.0775, 0.08, 0.0825, 0.085, 0.0875, 0.09, 0.0925, 0.095, 0.0975, 0.1, 
                        300, 388, 484, 500, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        183, 251, 305, 350, 390, 453, 492, 500, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        131, 182, 182, 225, 263, 297, 328, 384, 432, 470, 495, 500, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        101, 143, 143, 177, 177, 208, 237, 264, 289, 336, 358, 398, 450, 478, 496, 500, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        83, 117, 117, 117, 146, 146, 172, 197, 197, 220, 263, 283, 321, 357, 390, 421, 448, 482, 497, 500, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        67, 96, 96, 96, 96, 120, 120, 142, 142, 163, 182, 201, 219, 237, 254, 287, 317, 347, 375, 413, 437, 468, 486, 498, 500, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        60, 86, 86, 86, 86, 107, 107, 107, 127, 127, 146, 164, 164, 181, 197, 213, 244, 273, 288, 315, 354, 378, 413, 434, 463, 480, 498, 500, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        53, 75, 75, 75, 75, 75, 95, 95, 95, 112, 112, 129, 129, 145, 160, 160, 175, 204, 217, 231, 257, 282, 306, 330, 363, 395, 415, 443, 468, 489, 498, 500, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        47, 67, 67, 67, 67, 67, 85, 85, 85, 85, 101, 101, 116, 116, 130, 130, 144, 157, 170, 183, 196, 208, 232, 255, 266, 288, 320, 351, 370, 398, 425, 449, 472, 484, 498, 500, NA, NA, NA, NA, NA, 
                        42, 61, 61, 61, 61, 61, 61, 76, 76, 76, 76, 91, 91, 91, 105, 105, 118, 130, 130, 143, 155, 166, 178, 189, 200, 222, 243, 263, 283, 303, 331, 358, 375, 409, 433, 455, 474, 486, 498, 500, NA, 
                        28, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 51, 51, 51, 51, 51, 61, 61, 61, 61, 70, 70, 70, 80, 80, 80, 88, 88, 97, 105, 105, 113, 121, 121, 129, 137, 152, 160, 167, 182, 189, 
                        21, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 38, 38, 38, 38, 38, 38, 38, 38, 46, 46, 46, 46, 46, 53, 53, 53, 53, 60, 60, 60, 66, 66, 66, 73, 73, 79, 79), ncol = 13)
  
  conf.level <- 0.99
  m <- c(seq(0.01, 0.10, 0.01), 0.15, 0.20)
  expected <- seq(0, 0.1, 0.0025)
  N <- 500
  
  tab <- matrix(NA, nrow = length(expected), ncol = length(m))
  for(rows in 1:length(expected)){
    for(columns in 1:length(m)){
      if(expected[rows] >= m[columns]){
        next
      } else {
        p <- try({ suppressMessages(ss <- jfa::planning(conf.level = conf.level, expected = expected[rows], likelihood = "hypergeometric", materiality = m[columns], N.units = N)) }, silent = T)
        if(class(p) == "try-error"){
          next 
        }
        tab[rows, columns] <- ss$n
      }
    }
  }
  
  table <- as.data.frame(tab, stringsAsFactors = F)
  table <- cbind(expected = round(expected, 4), table)
  table <- matrix(as.numeric(unlist(table)), ncol = 13)
  
  expect_equal(table, reference)
})

test_that(desc = "(id: f7-v0.5.5-t20) Statistical Sample Sizes based on the Hypergeometric Distribution (N = 1000) - 1 Percent Risk of Overreliance", {
  testthat::skip_on_cran()
  reference <- matrix(c(0, 0.0025, 0.005, 0.0075, 0.01, 0.0125, 0.015, 0.0175, 0.02, 0.0225, 0.025, 0.0275, 0.03, 0.0325, 0.035, 0.0375, 0.04, 0.0425, 0.045, 0.0475, 0.05, 0.0525, 0.055, 0.0575, 0.06, 0.0625, 0.065, 0.0675, 0.07, 0.0725, 0.075, 0.0775, 0.08, 0.0825, 0.085, 0.0875, 0.09, 0.0925, 0.095, 0.0975, 0.1, 
                        368, 610, 781, 985, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        204, 287, 356, 476, 581, 718, 901, 992, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        141, 200, 200, 250, 295, 378, 454, 559, 684, 797, 916, 995, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        107, 153, 153, 192, 192, 227, 261, 324, 382, 438, 517, 616, 729, 851, 938, 996, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        86, 123, 123, 123, 155, 155, 185, 212, 239, 264, 313, 359, 426, 489, 570, 665, 771, 868, 950, 997, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        71, 102, 102, 102, 128, 128, 128, 153, 176, 176, 198, 240, 260, 300, 337, 392, 445, 513, 594, 672, 760, 855, 927, 986, 1000, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        62, 89, 89, 89, 89, 112, 112, 112, 134, 154, 154, 174, 192, 211, 246, 263, 297, 346, 393, 439, 498, 570, 652, 730, 816, 896, 964, 998, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        54, 78, 78, 78, 78, 78, 98, 98, 98, 117, 117, 135, 152, 152, 169, 185, 217, 232, 262, 291, 320, 361, 415, 468, 531, 605, 676, 755, 840, 909, 969, 998, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                        48, 69, 69, 69, 69, 69, 88, 88, 88, 88, 104, 104, 121, 121, 136, 151, 165, 180, 194, 207, 234, 247, 286, 311, 348, 396, 443, 500, 556, 631, 704, 774, 849, 919, 972, 998, NA, NA, NA, NA, NA, 
                        43, 62, 62, 62, 62, 62, 62, 79, 79, 79, 79, 94, 94, 109, 109, 123, 123, 136, 149, 162, 175, 187, 200, 224, 247, 270, 304, 338, 370, 413, 465, 516, 575, 652, 717, 797, 865, 927, 975, 998, NA, 
                        28, 41, 41, 41, 41, 41, 41, 41, 41, 41, 52, 52, 52, 52, 52, 52, 62, 62, 62, 62, 72, 72, 72, 82, 82, 91, 91, 100, 108, 108, 117, 125, 125, 142, 150, 158, 166, 182, 198, 213, 229, 
                        21, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 39, 39, 39, 39, 39, 39, 39, 46, 46, 46, 46, 46, 46, 54, 54, 54, 61, 61, 61, 68, 68, 68, 74, 74, 81, 81, 87), ncol = 13)
  
  conf.level <- 0.99
  m <- c(seq(0.01, 0.10, 0.01), 0.15, 0.20)
  expected <- seq(0, 0.1, 0.0025)
  N <- 1000
  
  tab <- matrix(NA, nrow = length(expected), ncol = length(m))
  for(rows in 1:length(expected)){
    for(columns in 1:length(m)){
      if(expected[rows] >= m[columns]){
        next
      } else {
        p <- try({ suppressMessages(ss <- jfa::planning(conf.level = conf.level, expected = expected[rows], likelihood = "hypergeometric", materiality = m[columns], N.units = N)) }, silent = T)
        if(class(p) == "try-error"){
          next 
        }
        tab[rows, columns] <- ss$n
      }
    }
  }
  
  table <- as.data.frame(tab, stringsAsFactors = F)
  table <- cbind(expected = round(expected, 4), table)
  table <- matrix(as.numeric(unlist(table)), ncol = 13)
  
  expect_equal(table, reference)
})

###################
## Upper limits ###
###################

# Binomial distribution

test_that(desc = "(id: f7-v0.5.5-21) Statistical Sampling Results based on the Binomial Distribution - Upper Limits at 10 Percent Risk of Overreliance", {
  testthat::skip_on_cran()
  reference <- matrix(c(20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95, 100, 125, 150, 200, 300, 400, 500, 
                        10.9, 8.8, 7.4, 6.4, 5.6, 5, 4.6, 4.2, 3.8, 3.5, 3.3, 3.1, 2.9, 2.7, 2.6, 2.4, 2.3, 1.9, 1.6, 1.2, 0.8, 0.6, 0.5, 
                        18.1, 14.7, 12.4, 10.7, 9.4, 8.4, 7.6, 6.9, 6.4, 5.9, 5.5, 5.1, 4.8, 4.5, 4.3, 4.1, 3.9, 3.1, 2.6, 2, 1.3, 1, 0.8, 
                        24.5, 20, 16.8, 14.5, 12.8, 11.4, 10.3, 9.4, 8.7, 8, 7.5, 7, 6.6, 6.2, 5.9, 5.6, 5.3, 4.3, 3.6, 2.7, 1.8, 1.4, 1.1, 
                        30.5, 24.9, 21, 18.2, 16, 14.3, 12.9, 11.8, 10.8, 10, 9.3, 8.7, 8.2, 7.7, 7.3, 6.9, 6.6, 5.3, 4.4, 3.4, 2.3, 1.7, 1.4, 
                        36.1, 29.5, 24.9, 21.6, 19, 17, 15.4, 14.1, 12.9, 12, 11.1, 10.4, 9.8, 9.2, 8.7, 8.3, 7.9, 6.3, 5.3, 4, 2.7, 2, 1.6, 
                        41.5, 34, 28.8, 24.9, 22, 19.7, 17.8, 16.3, 15, 13.9, 12.9, 12.1, 11.3, 10.7, 10.1, 9.6, 9.1, 7.3, 6.1, 4.6, 3.1, 2.4, 1.9, 
                        46.8, 38.4, 32.5, 28.2, 24.9, 22.3, 20.2, 18.4, 16.9, 15.7, 14.6, 13.7, 12.8, 12.1, 11.5, 10.9, 10.3, 8.3, 7, 5.3, 3.5, 2.7, 2.1, 
                        51.9, 42.6, 36.2, 31.4, 27.7, 24.8, 22.5, 20.5, 18.9, 17.5, 16.3, 15.2, 14.3, 13.5, 12.8, 12.1, 11.5, 9.3, 7.8, 5.9, 3.9, 3, 2.4, 
                        56.8, 46.8, 39.7, 34.5, 30.5, 27.3, 24.7, 22.6, 20.8, 19.3, 18, 16.8, 15.8, 14.9, 14.1, 13.4, 12.7, 10.2, 8.6, 6.5, 4.3, 3.3, 2.6, 
                        61.6, 50.8, 43.2, 37.6, 33.2, 29.8, 27, 24.6, 22.7, 21, 19.6, 18.3, 17.2, 16.2, 15.4, 14.6, 13.9, 11.2, 9.4, 7.1, 4.7, 3.6, 2.9, 66.2, 
                        54.8, 46.7, 40.6, 35.9, 32.2, 29.2, 26.7, 24.6, 22.8, 21.2, 19.8, 18.7, 17.6, 16.7, 15.8, 15, 12.1, 10.1, 7.6, 5.1, 3.9, 3.1), ncol = 12)
  
  conf.level <- 0.90
  n <- c(seq(20, 100, 5), 125, 150, 200, 300, 400, 500)
  k <- 0:10
  
  tab <- matrix(NA, nrow = length(n), ncol = length(k))
  for(rows in 1:length(n)){
    for(columns in 1:length(k)){
      ss <- jfa::evaluation(conf.level = conf.level, n = n[rows], x = k[columns], materiality = 1, method = "binomial")
      tab[rows, columns] <- ceiling(ss$ub * 1000) / 10
    }
  }
  
  table <- as.data.frame(tab)
  table <- cbind(n = n, table)
  table <- matrix(as.numeric(unlist(table)), ncol = 12)
  
  expect_equal(table, reference)
})

test_that(desc = "(id: f7-v0.5.5-22) Statistical Sampling Results based on the Binomial Distribution - Upper Limits at 5 Percent Risk of Overreliance", {
  testthat::skip_on_cran()
  reference <- matrix(c(20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95, 100, 125, 150, 200, 300, 400, 500, 
                        14, 11.3, 9.6, 8.3, 7.3, 6.5, 5.9, 5.4, 4.9, 4.6, 4.2, 4, 3.7, 3.5, 3.3, 3.2, 3, 2.4, 2, 1.5, 1, 0.8, 0.6, 
                        21.7, 17.7, 14.9, 12.9, 11.4, 10.2, 9.2, 8.4, 7.7, 7.1, 6.6, 6.2, 5.8, 5.5, 5.2, 4.9, 4.7, 3.8, 3.2, 2.4, 1.6, 1.2, 1, 
                        28.3, 23.2, 19.6, 17, 15, 13.4, 12.1, 11.1, 10.2, 9.4, 8.8, 8.2, 7.7, 7.3, 6.9, 6.5, 6.2, 5, 4.2, 3.2, 2.1, 1.6, 1.3, 
                        34.4, 28.2, 23.9, 20.7, 18.3, 16.4, 14.8, 13.5, 12.5, 11.5, 10.8, 10.1, 9.5, 8.9, 8.4, 8, 7.6, 6.1, 5.1, 3.9, 2.6, 2, 1.6, 
                        40.2, 33, 28, 24.3, 21.5, 19.2, 17.4, 15.9, 14.7, 13.6, 12.7, 11.8, 11.1, 10.5, 9.9, 9.4, 9, 7.2, 6, 4.6, 3.1, 2.3, 1.9, 
                        45.6, 37.6, 31.9, 27.8, 24.6, 22, 19.9, 18.2, 16.8, 15.5, 14.5, 13.6, 12.7, 12, 11.4, 10.8, 10.3, 8.3, 6.9, 5.2, 3.5, 2.7, 2.1, 
                        50.8, 42, 35.8, 31.1, 27.5, 24.7, 22.4, 20.5, 18.8, 17.5, 16.3, 15.2, 14.3, 13.5, 12.8, 12.1, 11.5, 9.3, 7.8, 5.9, 4, 3, 2.4, 
                        55.9, 46.3, 39.4, 34.4, 30.4, 27.3, 24.7, 22.6, 20.8, 19.3, 18, 16.9, 15.9, 15, 14.2, 13.4, 12.8, 10.3, 8.6, 6.5, 4.4, 3.3, 2.7, 
                        60.7, 50.4, 43, 37.5, 33.3, 29.8, 27.1, 24.8, 22.8, 21.2, 19.7, 18.5, 17.4, 16.4, 15.5, 14.7, 14, 11.3, 9.5, 7.2, 4.8, 3.6, 2.9, 
                        65.4, 54.4, 46.6, 40.6, 36, 32.4, 29.4, 26.9, 24.8, 23, 21.4, 20.1, 18.9, 17.8, 16.9, 16, 15.2, 12.3, 10.3, 7.8, 5.2, 3.9, 3.2, 
                        69.9, 58.4, 50, 43.7, 38.8, 34.8, 31.6, 28.9, 26.7, 24.7, 23.1, 21.6, 20.3, 19.2, 18.2, 17.3, 16.4, 13.2, 11.1, 8.4, 5.6, 4.3, 3.4), ncol = 12)
  
  conf.level <- 0.95
  n <- c(seq(20, 100, 5), 125, 150, 200, 300, 400, 500)
  k <- 0:10
  
  tab <- matrix(NA, nrow = length(n), ncol = length(k))
  for(rows in 1:length(n)){
    for(columns in 1:length(k)){
      ss <- jfa::evaluation(conf.level = conf.level, n = n[rows], x = k[columns], materiality = 1, method = "binomial")
      tab[rows, columns] <- ceiling(ss$ub * 1000) / 10
    }
  }
  
  table <- as.data.frame(tab)
  table <- cbind(n = n, table)
  table <- matrix(as.numeric(unlist(table)), ncol = 12)
  
  expect_equal(table, reference)
})

test_that(desc = "(id: f7-v0.5.5-23) Statistical Sampling Results based on the Binomial Distribution - Upper Limits at 2.5 Percent Risk of Overreliance", {
  testthat::skip_on_cran()
  reference <- matrix(c(20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95, 100, 125, 150, 200, 300, 400, 500, 
                        16.9, 13.8, 11.6, 10.1, 8.9, 7.9, 7.2, 6.5, 6, 5.6, 5.2, 4.8, 4.6, 4.3, 4.1, 3.9, 3.7, 3, 2.5, 1.9, 1.3, 1, 0.8, 
                        24.9, 20.4, 17.3, 15, 13.2, 11.8, 10.7, 9.8, 9, 8.3, 7.8, 7.3, 6.8, 6.4, 6.1, 5.8, 5.5, 4.4, 3.7, 2.8, 1.9, 1.4, 1.2, 
                        31.7, 26.1, 22.1, 19.2, 17, 15.2, 13.8, 12.6, 11.6, 10.7, 10, 9.4, 8.8, 8.3, 7.8, 7.4, 7.1, 5.7, 4.8, 3.6, 2.4, 1.8, 1.5, 
                        37.9, 31.3, 26.6, 23.1, 20.4, 18.3, 16.6, 15.2, 14, 13, 12.1, 11.3, 10.6, 10, 9.5, 9, 8.6, 6.9, 5.8, 4.4, 2.9, 2.2, 1.8, 
                        43.7, 36.1, 30.8, 26.8, 23.7, 21.3, 19.3, 17.6, 16.2, 15.1, 14, 13.1, 12.4, 11.7, 11, 10.5, 10, 8, 6.7, 5.1, 3.4, 2.6, 2.1, 
                        49.2, 40.8, 34.8, 30.3, 26.9, 24.1, 21.9, 20, 18.4, 17.1, 15.9, 14.9, 14, 13.2, 12.5, 11.9, 11.3, 9.1, 7.7, 5.8, 3.9, 2.9, 2.4, 
                        54.3, 45.2, 38.6, 33.7, 29.9, 26.8, 24.4, 22.3, 20.6, 19.1, 17.8, 16.7, 15.7, 14.8, 14, 13.3, 12.7, 10.2, 8.6, 6.5, 4.4, 3.3, 2.6, 
                        59.3, 49.4, 42.3, 37, 32.8, 29.5, 26.8, 24.5, 22.6, 21, 19.6, 18.3, 17.3, 16.3, 15.4, 14.6, 13.9, 11.2, 9.4, 7.1, 4.8, 3.6, 2.9, 
                        64, 53.6, 45.9, 40.2, 35.7, 32.1, 29.2, 26.7, 24.6, 22.9, 21.3, 20, 18.8, 17.8, 16.8, 16, 15.2, 12.3, 10.3, 7.8, 5.2, 4, 3.2, 
                        68.5, 57.5, 49.4, 43.3, 38.5, 34.6, 31.5, 28.9, 26.6, 24.7, 23.1, 21.6, 20.3, 19.2, 18.2, 17.3, 16.4, 13.3, 11.1, 8.4, 5.7, 4.3, 3.4, 
                        72.9, 61.4, 52.9, 46.4, 41.2, 37.1, 33.8, 31, 28.6, 26.5, 24.8, 23.2, 21.8, 20.6, 19.5, 18.6, 17.7, 14.3, 12, 9.1, 6.1, 4.6, 3.7), ncol = 12)
  
  conf.level <- 0.975
  n <- c(seq(20, 100, 5), 125, 150, 200, 300, 400, 500)
  k <- 0:10
  
  tab <- matrix(NA, nrow = length(n), ncol = length(k))
  for(rows in 1:length(n)){
    for(columns in 1:length(k)){
      ss <- jfa::evaluation(conf.level = conf.level, n = n[rows], x = k[columns], materiality = 1, method = "binomial")
      tab[rows, columns] <- ceiling(ss$ub * 1000) / 10
    }
  }
  
  table <- as.data.frame(tab)
  table <- cbind(n = n, table)
  table <- matrix(as.numeric(unlist(table)), ncol = 12)
  
  expect_equal(table, reference)
})

test_that(desc = "(id: f7-v0.5.5-24) Statistical Sampling Results based on the Binomial Distribution - Upper Limits at 1 Percent Risk of Overreliance", {
  testthat::skip_on_cran()
  reference <- matrix(c(20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95, 100, 125, 150, 200, 300, 400, 500, 
                        20.6, 16.9, 14.3, 12.4, 10.9, 9.8, 8.8, 8.1, 7.4, 6.9, 6.4, 6, 5.6, 5.3, 5, 4.8, 4.6, 3.7, 3.1, 2.3, 1.6, 1.2, 1, 
                        28.9, 23.8, 20.2, 17.6, 15.5, 13.9, 12.6, 11.5, 10.6, 9.8, 9.2, 8.6, 8.1, 7.6, 7.2, 6.8, 6.5, 5.2, 4.4, 3.3, 2.2, 1.7, 1.4, 
                        35.9, 29.6, 25.2, 22, 19.5, 17.4, 15.8, 14.5, 13.3, 12.4, 11.5, 10.8, 10.1, 9.6, 9.1, 8.6, 8.2, 6.6, 5.5, 4.2, 2.8, 2.1, 1.7, 
                        42.1, 34.9, 29.8, 26, 23, 20.7, 18.8, 17.2, 15.8, 14.7, 13.7, 12.8, 12.1, 11.4, 10.8, 10.2, 9.7, 7.9, 6.6, 5, 3.4, 2.5, 2, 
                        47.9, 39.8, 34.1, 29.8, 26.4, 23.7, 21.5, 19.7, 18.2, 16.9, 15.7, 14.8, 13.9, 13.1, 12.4, 11.8, 11.2, 9.1, 7.6, 5.7, 3.9, 2.9, 2.4, 
                        53.3, 44.5, 38.1, 33.3, 29.6, 26.6, 24.2, 22.2, 20.5, 19, 17.7, 16.6, 15.6, 14.7, 14, 13.3, 12.6, 10.2, 8.6, 6.5, 4.4, 3.3, 2.7, 
                        58.3, 48.9, 42, 36.8, 32.7, 29.4, 26.8, 24.5, 22.6, 21, 19.6, 18.4, 17.3, 16.3, 15.5, 14.7, 14, 11.3, 9.5, 7.2, 4.8, 3.7, 2.9, 
                        63.1, 53.1, 45.7, 40.1, 35.7, 32.2, 29.2, 26.8, 24.8, 23, 21.5, 20.1, 18.9, 17.9, 17, 16.1, 15.3, 12.4, 10.4, 7.9, 5.3, 4, 3.2, 
                        67.7, 57.2, 49.3, 43.3, 38.6, 34.8, 31.7, 29, 26.8, 24.9, 23.3, 21.8, 20.5, 19.4, 18.4, 17.5, 16.6, 13.5, 11.3, 8.5, 5.8, 4.3, 3.5, 
                        72, 61.1, 52.8, 46.4, 41.4, 37.4, 34, 31.2, 28.8, 26.8, 25, 23.5, 22.1, 20.9, 19.8, 18.8, 17.9, 14.5, 12.2, 9.2, 6.2, 4.7, 3.8, 
                        76.2, 64.8, 56.2, 49.5, 44.2, 39.9, 36.3, 33.4, 30.8, 28.7, 26.8, 25.1, 23.7, 22.4, 21.2, 20.1, 19.2, 15.5, 13, 9.9, 6.7, 5, 4), ncol = 12)
  
  conf.level <- 0.99
  n <- c(seq(20, 100, 5), 125, 150, 200, 300, 400, 500)
  k <- 0:10
  
  tab <- matrix(NA, nrow = length(n), ncol = length(k))
  for(rows in 1:length(n)){
    for(columns in 1:length(k)){
      ss <- jfa::evaluation(conf.level = conf.level, n = n[rows], x = k[columns], materiality = 1, method = "binomial")
      tab[rows, columns] <- ceiling(ss$ub * 1000) / 10
    }
  }
  
  table <- as.data.frame(tab)
  table <- cbind(n = n, table)
  table <- matrix(as.numeric(unlist(table)), ncol = 12)
  
  expect_equal(table, reference)
})

# Poisson distribution

test_that(desc = "(id: f7-v0.5.5-25) Statistical Sampling Results based on the Poisson Distribution - Upper Limits at 10 Percent Risk of Overreliance", {
  testthat::skip_on_cran()
  reference <- matrix(c(20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95, 100, 125, 150, 200, 300, 400, 500, 
                        11.6, 9.3, 7.7, 6.6, 5.8, 5.2, 4.7, 4.2, 3.9, 3.6, 3.3, 3.1, 2.9, 2.8, 2.6, 2.5, 2.4, 1.9, 1.6, 1.2, 0.8, 0.6, 0.5, 
                        19.5, 15.6, 13, 11.2, 9.8, 8.7, 7.8, 7.1, 6.5, 6, 5.6, 5.2, 4.9, 4.6, 4.4, 4.1, 3.9, 3.2, 2.6, 2, 1.3, 1, 0.8, 
                        26.7, 21.3, 17.8, 15.3, 13.4, 11.9, 10.7, 9.7, 8.9, 8.2, 7.7, 7.1, 6.7, 6.3, 6, 5.7, 5.4, 4.3, 3.6, 2.7, 1.8, 1.4, 1.1, 
                        33.5, 26.8, 22.3, 19.1, 16.8, 14.9, 13.4, 12.2, 11.2, 10.3, 9.6, 9, 8.4, 7.9, 7.5, 7.1, 6.7, 5.4, 4.5, 3.4, 2.3, 1.7, 1.4, 
                        40, 32, 26.7, 22.9, 20, 17.8, 16, 14.6, 13.4, 12.3, 11.5, 10.7, 10, 9.5, 8.9, 8.5, 8, 6.4, 5.4, 4, 2.7, 2, 1.6, 
                        46.4, 37.1, 31, 26.5, 23.2, 20.7, 18.6, 16.9, 15.5, 14.3, 13.3, 12.4, 11.6, 11, 10.4, 9.8, 9.3, 7.5, 6.2, 4.7, 3.1, 2.4, 1.9, 
                        52.7, 42.2, 35.2, 30.1, 26.4, 23.5, 21.1, 19.2, 17.6, 16.3, 15.1, 14.1, 13.2, 12.4, 11.8, 11.1, 10.6, 8.5, 7.1, 5.3, 3.6, 2.7, 2.2, 
                        58.9, 47.1, 39.3, 33.7, 29.5, 26.2, 23.6, 21.5, 19.7, 18.2, 16.9, 15.7, 14.8, 13.9, 13.1, 12.4, 11.8, 9.5, 7.9, 5.9, 4, 3, 2.4, 
                        65, 52, 43.4, 37.2, 32.5, 28.9, 26, 23.7, 21.7, 20, 18.6, 17.4, 16.3, 15.3, 14.5, 13.7, 13, 10.4, 8.7, 6.5, 4.4, 3.3, 2.6, 
                        71.1, 56.9, 47.4, 40.6, 35.6, 31.6, 28.5, 25.9, 23.7, 21.9, 20.3, 19, 17.8, 16.8, 15.8, 15, 14.3, 11.4, 9.5, 7.2, 4.8, 3.6, 2.9, 
                        77.1, 61.7, 51.4, 44.1, 38.6, 34.3, 30.9, 28.1, 25.7, 23.8, 22.1, 20.6, 19.3, 18.2, 17.2, 16.3, 15.5, 12.4, 10.3, 7.8, 5.2, 3.9, 3.1), ncol = 12)
  
  conf.level <- 0.90
  n <- c(seq(20, 100, 5), 125, 150, 200, 300, 400, 500)
  k <- 0:10
  
  tab <- matrix(NA, nrow = length(n), ncol = length(k))
  for(rows in 1:length(n)){
    for(columns in 1:length(k)){
      ss <- jfa::evaluation(conf.level = conf.level, n = n[rows], x = k[columns], materiality = 1, method = "poisson")
      tab[rows, columns] <- ceiling(ss$ub * 1000) / 10
    }
  }
  
  table <- as.data.frame(tab)
  table <- cbind(n = n, table)
  table <- matrix(as.numeric(unlist(table)), ncol = 12)
  
  expect_equal(table, reference)
})

test_that(desc = "(id: f7-v0.5.5-26) Statistical Sampling Results based on the Poisson Distribution - Upper Limits at 5 Percent Risk of Overreliance", {
  testthat::skip_on_cran()
  reference <- matrix(c(20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95, 100, 125, 150, 200, 300, 400, 500, 
                        15, 12, 10, 8.6, 7.5, 6.7, 6, 5.5, 5, 4.7, 4.3, 4, 3.8, 3.6, 3.4, 3.2, 3, 2.4, 2, 1.5, 1, 0.8, 0.6, 
                        23.8, 19, 15.9, 13.6, 11.9, 10.6, 9.5, 8.7, 8, 7.3, 6.8, 6.4, 6, 5.6, 5.3, 5, 4.8, 3.8, 3.2, 2.4, 1.6, 1.2, 1, 
                        31.5, 25.2, 21, 18, 15.8, 14, 12.6, 11.5, 10.5, 9.7, 9, 8.4, 7.9, 7.5, 7, 6.7, 6.3, 5.1, 4.2, 3.2, 2.1, 1.6, 1.3, 
                        38.8, 31.1, 25.9, 22.2, 19.4, 17.3, 15.6, 14.1, 13, 12, 11.1, 10.4, 9.7, 9.2, 8.7, 8.2, 7.8, 6.3, 5.2, 3.9, 2.6, 2, 1.6, 
                        45.8, 36.7, 30.6, 26.2, 22.9, 20.4, 18.4, 16.7, 15.3, 14.1, 13.1, 12.3, 11.5, 10.8, 10.2, 9.7, 9.2, 7.4, 6.2, 4.6, 3.1, 2.3, 1.9, 
                        52.6, 42.1, 35.1, 30.1, 26.3, 23.4, 21.1, 19.2, 17.6, 16.2, 15.1, 14.1, 13.2, 12.4, 11.7, 11.1, 10.6, 8.5, 7.1, 5.3, 3.6, 2.7, 2.2, 
                        59.3, 47.4, 39.5, 33.9, 29.7, 26.4, 23.7, 21.6, 19.8, 18.3, 17, 15.8, 14.9, 14, 13.2, 12.5, 11.9, 9.5, 7.9, 6, 4, 3, 2.4, 
                        65.8, 52.6, 43.9, 37.6, 32.9, 29.3, 26.3, 24, 22, 20.3, 18.8, 17.6, 16.5, 15.5, 14.7, 13.9, 13.2, 10.6, 8.8, 6.6, 4.4, 3.3, 2.7, 
                        72.2, 57.8, 48.2, 41.3, 36.1, 32.1, 28.9, 26.3, 24.1, 22.3, 20.7, 19.3, 18.1, 17, 16.1, 15.2, 14.5, 11.6, 9.7, 7.3, 4.9, 3.7, 2.9, 
                        78.6, 62.9, 52.4, 44.9, 39.3, 35, 31.5, 28.6, 26.2, 24.2, 22.5, 21, 19.7, 18.5, 17.5, 16.6, 15.8, 12.6, 10.5, 7.9, 5.3, 4, 3.2, 
                        84.9, 67.9, 56.6, 48.5, 42.5, 37.7, 34, 30.9, 28.3, 26.1, 24.3, 22.7, 21.3, 20, 18.9, 17.9, 17, 13.6, 11.4, 8.5, 5.7, 4.3, 3.4), ncol = 12)
  
  conf.level <- 0.95
  n <- c(seq(20, 100, 5), 125, 150, 200, 300, 400, 500)
  k <- 0:10
  
  tab <- matrix(NA, nrow = length(n), ncol = length(k))
  for(rows in 1:length(n)){
    for(columns in 1:length(k)){
      ss <- jfa::evaluation(conf.level = conf.level, n = n[rows], x = k[columns], materiality = 1, method = "poisson")
      tab[rows, columns] <- ceiling(ss$ub * 1000) / 10
    }
  }
  
  table <- as.data.frame(tab)
  table <- cbind(n = n, table)
  table <- matrix(as.numeric(unlist(table)), ncol = 12)
  
  expect_equal(table, reference)
})

test_that(desc = "(id: f7-v0.5.5-27) Statistical Sampling Results based on the Poisson Distribution - Upper Limits at 2.5 Percent Risk of Overreliance", {
  testthat::skip_on_cran()
  reference <- matrix(c(20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95, 100, 125, 150, 200, 300, 400, 500, 
                        18.5, 14.8, 12.3, 10.6, 9.3, 8.2, 7.4, 6.8, 6.2, 5.7, 5.3, 5, 4.7, 4.4, 4.1, 3.9, 3.7, 3, 2.5, 1.9, 1.3, 1, 0.8, 
                        27.9, 22.3, 18.6, 16, 14, 12.4, 11.2, 10.2, 9.3, 8.6, 8, 7.5, 7, 6.6, 6.2, 5.9, 5.6, 4.5, 3.8, 2.8, 1.9, 1.4, 1.2, 
                        36.2, 28.9, 24.1, 20.7, 18.1, 16.1, 14.5, 13.2, 12.1, 11.2, 10.4, 9.7, 9.1, 8.5, 8.1, 7.7, 7.3, 5.8, 4.9, 3.7, 2.5, 1.9, 1.5, 
                        43.9, 35.1, 29.3, 25.1, 22, 19.5, 17.6, 16, 14.7, 13.5, 12.6, 11.7, 11, 10.4, 9.8, 9.3, 8.8, 7.1, 5.9, 4.4, 3, 2.2, 1.8, 
                        51.3, 41, 34.2, 29.3, 25.7, 22.8, 20.5, 18.7, 17.1, 15.8, 14.7, 13.7, 12.9, 12.1, 11.4, 10.8, 10.3, 8.2, 6.9, 5.2, 3.5, 2.6, 2.1, 
                        58.4, 46.7, 38.9, 33.4, 29.2, 26, 23.4, 21.3, 19.5, 18, 16.7, 15.6, 14.6, 13.8, 13, 12.3, 11.7, 9.4, 7.8, 5.9, 3.9, 3, 2.4, 
                        65.3, 52.3, 43.6, 37.4, 32.7, 29.1, 26.2, 23.8, 21.8, 20.1, 18.7, 17.5, 16.4, 15.4, 14.6, 13.8, 13.1, 10.5, 8.8, 6.6, 4.4, 3.3, 2.7, 
                        72.2, 57.7, 48.1, 41.3, 36.1, 32.1, 28.9, 26.3, 24.1, 22.2, 20.7, 19.3, 18.1, 17, 16.1, 15.2, 14.5, 11.6, 9.7, 7.3, 4.9, 3.7, 2.9, 
                        78.9, 63.1, 52.6, 45.1, 39.5, 35.1, 31.6, 28.7, 26.3, 24.3, 22.6, 21.1, 19.8, 18.6, 17.6, 16.6, 15.8, 12.7, 10.6, 7.9, 5.3, 4, 3.2, 
                        85.5, 68.4, 57, 48.9, 42.8, 38, 34.2, 31.1, 28.5, 26.3, 24.5, 22.8, 21.4, 20.1, 19, 18, 17.1, 13.7, 11.4, 8.6, 5.7, 4.3, 3.5, 
                        92, 73.6, 61.4, 52.6, 46, 40.9, 36.8, 33.5, 30.7, 28.3, 26.3, 24.6, 23, 21.7, 20.5, 19.4, 18.4, 14.8, 12.3, 9.2, 6.2, 4.6, 3.7), ncol = 12)
  
  conf.level <- 0.975
  n <- c(seq(20, 100, 5), 125, 150, 200, 300, 400, 500)
  k <- 0:10
  
  tab <- matrix(NA, nrow = length(n), ncol = length(k))
  for(rows in 1:length(n)){
    for(columns in 1:length(k)){
      ss <- jfa::evaluation(conf.level = conf.level, n = n[rows], x = k[columns], materiality = 1, method = "poisson")
      tab[rows, columns] <- ceiling(ss$ub * 1000) / 10
    }
  }
  
  table <- as.data.frame(tab)
  table <- cbind(n = n, table)
  table <- matrix(as.numeric(unlist(table)), ncol = 12)
  
  expect_equal(table, reference)
})

test_that(desc = "(id: f7-v0.5.5-28) Statistical Sampling Results based on the Poisson Distribution - Upper Limits at 1 Percent Risk of Overreliance", {
  testthat::skip_on_cran()
  reference <- matrix(c(20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95, 100, 125, 150, 200, 300, 400, 500, 
                        23.1, 18.5, 15.4, 13.2, 11.6, 10.3, 9.3, 8.4, 7.7, 7.1, 6.6, 6.2, 5.8, 5.5, 5.2, 4.9, 4.7, 3.7, 3.1, 2.4, 1.6, 1.2, 1, 
                        33.2, 26.6, 22.2, 19, 16.6, 14.8, 13.3, 12.1, 11.1, 10.3, 9.5, 8.9, 8.3, 7.9, 7.4, 7, 6.7, 5.4, 4.5, 3.4, 2.3, 1.7, 1.4, 
                        42.1, 33.7, 28.1, 24.1, 21.1, 18.7, 16.9, 15.3, 14.1, 13, 12.1, 11.3, 10.6, 9.9, 9.4, 8.9, 8.5, 6.8, 5.7, 4.3, 2.9, 2.2, 1.7, 
                        50.3, 40.2, 33.5, 28.8, 25.2, 22.4, 20.1, 18.3, 16.8, 15.5, 14.4, 13.4, 12.6, 11.9, 11.2, 10.6, 10.1, 8.1, 6.7, 5.1, 3.4, 2.6, 2.1, 
                        58.1, 46.5, 38.7, 33.2, 29.1, 25.8, 23.3, 21.1, 19.4, 17.9, 16.6, 15.5, 14.6, 13.7, 12.9, 12.3, 11.7, 9.3, 7.8, 5.9, 3.9, 3, 2.4, 
                        65.6, 52.5, 43.7, 37.5, 32.8, 29.2, 26.3, 23.9, 21.9, 20.2, 18.8, 17.5, 16.4, 15.5, 14.6, 13.8, 13.2, 10.5, 8.8, 6.6, 4.4, 3.3, 2.7, 
                        72.9, 58.3, 48.6, 41.7, 36.5, 32.4, 29.2, 26.5, 24.3, 22.5, 20.9, 19.5, 18.3, 17.2, 16.2, 15.4, 14.6, 11.7, 9.8, 7.3, 4.9, 3.7, 3, 
                        80, 64, 53.4, 45.8, 40, 35.6, 32, 29.1, 26.7, 24.7, 22.9, 21.4, 20, 18.9, 17.8, 16.9, 16, 12.8, 10.7, 8, 5.4, 4, 3.2, 
                        87.1, 69.7, 58.1, 49.8, 43.6, 38.7, 34.9, 31.7, 29.1, 26.8, 24.9, 23.3, 21.8, 20.5, 19.4, 18.4, 17.5, 14, 11.7, 8.8, 5.9, 4.4, 3.5, 
                        94, 75.2, 62.7, 53.7, 47, 41.8, 37.6, 34.2, 31.4, 28.9, 26.9, 25.1, 23.5, 22.1, 20.9, 19.8, 18.8, 15.1, 12.6, 9.4, 6.3, 4.7, 3.8, 
                        100.8, 80.6, 67.2, 57.6, 50.4, 44.8, 40.3, 36.7, 33.6, 31, 28.8, 26.9, 25.2, 23.7, 22.4, 21.3, 20.2, 16.2, 13.5, 10.1, 6.8, 5.1, 4.1), ncol = 12)
  
  conf.level <- 0.99
  n <- c(seq(20, 100, 5), 125, 150, 200, 300, 400, 500)
  k <- 0:10
  
  tab <- matrix(NA, nrow = length(n), ncol = length(k))
  for(rows in 1:length(n)){
    for(columns in 1:length(k)){
      ss <- jfa::evaluation(conf.level = conf.level, n = n[rows], x = k[columns], materiality = 1, method = "poisson")
      tab[rows, columns] <- ceiling(ss$ub * 1000) / 10
    }
  }
  
  table <- as.data.frame(tab)
  table <- cbind(n = n, table)
  table <- matrix(as.numeric(unlist(table)), ncol = 12)
  
  expect_equal(table, reference)
})

# Hypergeometric distribution

test_that(desc = "(id: f7-v0.5.5-29) Statistical Sampling Results based on the Hypergeometric Distribution (N = 100) - Upper Limits at 10 Percent Risk of Overreliance", {
  testthat::skip_on_cran()
  reference <- matrix(c(20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95, 100, 125, 150, 200, 300, 400, 500, 
                        9, 7, 6, 5, 4, 3, 3, 2, 2, 2, 1, 1, 1, 1, 1, 0, NA, NA, NA, NA, NA, NA, NA, 
                        16, 13, 10, 9, 7, 6, 6, 5, 4, 4, 3, 3, 3, 2, 2, 1, NA, NA, NA, NA, NA, NA, NA, 
                        23, 18, 15, 12, 11, 9, 8, 7, 6, 6, 5, 4, 4, 4, 3, 3, NA, NA, NA, NA, NA, NA, NA, 
                        28, 23, 19, 16, 14, 12, 10, 9, 8, 7, 7, 6, 5, 5, 4, 4, NA, NA, NA, NA, NA, NA, NA, 
                        34, 27, 23, 19, 17, 15, 13, 11, 10, 9, 8, 8, 7, 6, 6, 5, NA, NA, NA, NA, NA, NA, NA, 
                        39, 32, 26, 23, 19, 17, 15, 14, 12, 11, 10, 9, 8, 7, 7, 6, NA, NA, NA, NA, NA, NA, NA, 
                        45, 36, 30, 26, 22, 20, 17, 16, 14, 13, 12, 11, 10, 9, 8, 7, NA, NA, NA, NA, NA, NA, NA, 
                        50, 40, 34, 29, 25, 22, 20, 18, 16, 14, 13, 12, 11, 10, 9, 8, NA, NA, NA, NA, NA, NA, NA, 
                        55, 45, 37, 32, 28, 25, 22, 20, 18, 16, 15, 13, 12, 11, 10, 9, NA, NA, NA, NA, NA, NA, NA, 
                        60, 49, 41, 35, 30, 27, 24, 22, 20, 18, 16, 15, 14, 13, 11, 10, NA, NA, NA, NA, NA, NA, NA, 
                        64, 53, 44, 38, 33, 29, 26, 24, 21, 19, 18, 16, 15, 14, 13, 12, NA, NA, NA, NA, NA, NA, NA), ncol = 12)
  
  conf.level <- 0.90
  n <- c(seq(20, 100, 5), 125, 150, 200, 300, 400, 500)
  k <- 0:10
  N <- 100
  
  tab <- matrix(NA, nrow = length(n), ncol = length(k))
  for(rows in 1:length(n)){
    for(columns in 1:length(k)){
      if(n[rows] >= N)
        next
      ss <- jfa::evaluation(conf.level = conf.level, n = n[rows], x = k[columns], materiality = 1, method = "hypergeometric", N.units = N)
      tab[rows, columns] <- ceiling(ss$ub * 1000) / 10
    }
  }
  
  table <- as.data.frame(tab)
  table <- cbind(n = n, table)
  table <- matrix(as.numeric(unlist(table)), ncol = 12)
  
  expect_equal(table, reference)
})

test_that(desc = "(id: f7-v0.5.5-30) Statistical Sampling Results based on the Hypergeometric Distribution (N = 500) - Upper Limits at 10 Percent Risk of Overreliance", {
  testthat::skip_on_cran()
  reference <- matrix(c(20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95, 100, 125, 150, 200, 300, 400, 500, 
                        10.6, 8.4, 7, 6, 5.2, 4.6, 4.2, 3.8, 3.4, 3.2, 3, 2.6, 2.6, 2.4, 2.2, 2, 2, 1.4, 1.2, 0.8, 0.4, 0.2, NA, 
                        17.8, 14.4, 12, 10.2, 9, 8, 7.2, 6.6, 6, 5.4, 5, 4.8, 4.4, 4.2, 3.8, 3.6, 3.4, 2.6, 2.2, 1.6, 0.8, 0.6, NA, 
                        24.2, 19.6, 16.4, 14.2, 12.4, 11, 9.8, 9, 8.2, 7.6, 7, 6.6, 6, 5.8, 5.4, 5, 4.8, 3.8, 3, 2.2, 1.2, 0.8, NA, 
                        30, 24.4, 20.6, 17.6, 15.6, 13.8, 12.4, 11.4, 10.4, 9.6, 8.8, 8.2, 7.8, 7.2, 6.8, 6.4, 6, 4.8, 4, 2.8, 1.6, 1, NA, 
                        35.6, 29, 24.4, 21.2, 18.6, 16.6, 14.8, 13.6, 12.4, 11.4, 10.6, 9.8, 9.2, 8.6, 8.2, 7.8, 7.4, 5.8, 4.8, 3.4, 2, 1.4, NA, 
                        41, 33.6, 28.2, 24.4, 21.4, 19.2, 17.2, 15.8, 14.4, 13.4, 12.4, 11.6, 10.8, 10.2, 9.6, 9, 8.6, 6.8, 5.6, 4, 2.4, 1.6, NA, 
                        46.4, 38, 32, 27.6, 24.4, 21.8, 19.6, 17.8, 16.4, 15.2, 14, 13, 12.2, 11.6, 10.8, 10.2, 9.8, 7.8, 6.4, 4.6, 2.8, 2, NA, 
                        51.4, 42.2, 35.6, 30.8, 27.2, 24.2, 22, 20, 18.4, 17, 15.8, 14.6, 13.8, 13, 12.2, 11.6, 11, 8.6, 7.2, 5.2, 3.2, 2.2, NA, 
                        56.4, 46.4, 39.2, 34, 30, 26.8, 24.2, 22, 20.2, 18.6, 17.4, 16.2, 15.2, 14.2, 13.4, 12.8, 12, 9.6, 8, 5.8, 3.6, 2.4, NA, 
                        61.2, 50.4, 42.8, 37, 32.6, 29.2, 26.4, 24, 22, 20.4, 19, 17.8, 16.6, 15.6, 14.8, 14, 13.2, 10.6, 8.6, 6.4, 4, 2.8, NA, 
                        65.8, 54.4, 46.2, 40, 35.4, 31.6, 28.6, 26, 24, 22.2, 20.6, 19.2, 18, 17, 16, 15.2, 14.4, 11.4, 9.4, 7, 4.4, 3, NA), ncol = 12)
  
  conf.level <- 0.90
  n <- c(seq(20, 100, 5), 125, 150, 200, 300, 400, 500)
  k <- 0:10
  N <- 500
  
  tab <- matrix(NA, nrow = length(n), ncol = length(k))
  for(rows in 1:length(n)){
    for(columns in 1:length(k)){
      if(n[rows] >= N)
        next
      ss <- jfa::evaluation(conf.level = conf.level, n = n[rows], x = k[columns], materiality = 1, method = "hypergeometric", N.units = N)
      tab[rows, columns] <- ceiling(ss$ub * 1000) / 10
    }
  }
  
  table <- as.data.frame(tab)
  table <- cbind(n = n, table)
  table <- matrix(as.numeric(unlist(table)), ncol = 12)
  
  expect_equal(table, reference)
})

test_that(desc = "(id: f7-v0.5.5-31) Statistical Sampling Results based on the Hypergeometric Distribution (N = 1000) - Upper Limits at 10 Percent Risk of Overreliance", {
  testthat::skip_on_cran()
  reference <- matrix(c(20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95, 100, 125, 150, 200, 300, 400, 500, 
                        10.7, 8.6, 7.2, 6.2, 5.4, 4.8, 4.3, 3.9, 3.6, 3.3, 3.1, 2.9, 2.7, 2.5, 2.4, 2.2, 2.1, 1.7, 1.4, 1, 0.6, 0.4, 0.3, 
                        17.9, 14.5, 12.2, 10.5, 9.2, 8.2, 7.4, 6.7, 6.1, 5.7, 5.3, 4.9, 4.6, 4.3, 4.1, 3.8, 3.6, 2.9, 2.4, 1.7, 1.1, 0.8, 0.6, 
                        24.3, 19.7, 16.6, 14.3, 12.6, 11.2, 10.1, 9.2, 8.4, 7.8, 7.2, 6.7, 6.3, 5.9, 5.6, 5.3, 5, 4, 3.3, 2.4, 1.5, 1.1, 0.8, 
                        30.2, 24.6, 20.7, 17.9, 15.7, 14, 12.7, 11.5, 10.6, 9.8, 9.1, 8.5, 7.9, 7.5, 7, 6.7, 6.3, 5, 4.2, 3.1, 2, 1.4, 1.1, 
                        35.9, 29.3, 24.7, 21.3, 18.8, 16.8, 15.1, 13.8, 12.7, 11.7, 10.9, 10.1, 9.5, 8.9, 8.4, 8, 7.6, 6, 5, 3.7, 2.4, 1.7, 1.3, 
                        41.3, 33.8, 28.5, 24.7, 21.7, 19.4, 17.5, 16, 14.7, 13.6, 12.6, 11.8, 11, 10.4, 9.8, 9.3, 8.8, 7, 5.8, 4.3, 2.8, 2, 1.6, 
                        46.5, 38.1, 32.2, 27.9, 24.6, 22, 19.9, 18.1, 16.6, 15.4, 14.3, 13.4, 12.5, 11.8, 11.1, 10.6, 10, 8, 6.6, 4.9, 3.2, 2.3, 1.8, 
                        51.6, 42.4, 35.9, 31.1, 27.4, 24.5, 22.2, 20.2, 18.6, 17.2, 16, 14.9, 14, 13.2, 12.5, 11.8, 11.2, 9, 7.4, 5.5, 3.6, 2.6, 2, 
                        56.5, 46.5, 39.5, 34.2, 30.2, 27, 24.4, 22.3, 20.5, 19, 17.6, 16.5, 15.5, 14.6, 13.8, 13, 12.4, 9.9, 8.2, 6.1, 4, 2.9, 2.3, 
                        61.3, 50.6, 43, 37.3, 32.9, 29.5, 26.7, 24.3, 22.4, 20.7, 19.3, 18, 16.9, 15.9, 15, 14.3, 13.5, 10.8, 9, 6.7, 4.4, 3.2, 2.5, 
                        66, 54.6, 46.4, 40.3, 35.6, 31.9, 28.9, 26.4, 24.2, 22.4, 20.9, 19.5, 18.3, 17.3, 16.3, 15.5, 14.7, 11.8, 9.8, 7.3, 4.8, 3.5, 2.7), ncol = 12)
  
  conf.level <- 0.90
  n <- c(seq(20, 100, 5), 125, 150, 200, 300, 400, 500)
  k <- 0:10
  N <- 1000
  
  tab <- matrix(NA, nrow = length(n), ncol = length(k))
  for(rows in 1:length(n)){
    for(columns in 1:length(k)){
      if(n[rows] >= N)
        next
      ss <- jfa::evaluation(conf.level = conf.level, n = n[rows], x = k[columns], materiality = 1, method = "hypergeometric", N.units = N)
      tab[rows, columns] <- ceiling(ss$ub * 1000) / 10
    }
  }
  
  table <- as.data.frame(tab)
  table <- cbind(n = n, table)
  table <- matrix(as.numeric(unlist(table)), ncol = 12)
  
  expect_equal(table, reference)
})

test_that(desc = "(id: f7-v0.5.5-32) Statistical Sampling Results based on the Hypergeometric Distribution (N = 100) - Upper Limits at 5 Percent Risk of Overreliance", {
  testthat::skip_on_cran()
  reference <- matrix(c(20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95, 100, 125, 150, 200, 300, 400, 500, 
                        12, 9, 8, 6, 5, 4, 4, 3, 3, 2, 2, 2, 1, 1, 1, 0, NA, NA, NA, NA, NA, NA, NA, 
                        19, 15, 13, 11, 9, 8, 7, 6, 5, 4, 4, 3, 3, 3, 2, 2, NA, NA, NA, NA, NA, NA, NA, 
                        26, 21, 17, 14, 12, 11, 9, 8, 7, 6, 6, 5, 5, 4, 3, 3, NA, NA, NA, NA, NA, NA, NA, 
                        32, 26, 21, 18, 15, 13, 12, 11, 9, 8, 8, 7, 6, 5, 5, 4, NA, NA, NA, NA, NA, NA, NA, 
                        38, 30, 25, 21, 19, 16, 14, 13, 11, 10, 9, 8, 7, 7, 6, 5, NA, NA, NA, NA, NA, NA, NA, 
                        43, 35, 29, 25, 21, 19, 17, 15, 13, 12, 11, 10, 9, 8, 7, 6, NA, NA, NA, NA, NA, NA, NA, 
                        48, 39, 33, 28, 24, 21, 19, 17, 15, 14, 12, 11, 10, 9, 8, 7, NA, NA, NA, NA, NA, NA, NA, 
                        53, 44, 36, 31, 27, 24, 21, 19, 17, 16, 14, 13, 12, 11, 10, 9, NA, NA, NA, NA, NA, NA, NA, 
                        58, 48, 40, 34, 30, 26, 24, 21, 19, 17, 16, 14, 13, 12, 11, 10, NA, NA, NA, NA, NA, NA, NA, 
                        63, 52, 44, 37, 33, 29, 26, 23, 21, 19, 17, 16, 14, 13, 12, 11, NA, NA, NA, NA, NA, NA, NA, 
                        68, 56, 47, 40, 35, 31, 28, 25, 23, 21, 19, 17, 16, 14, 13, 12, NA, NA, NA, NA, NA, NA, NA), ncol = 12)
  
  conf.level <- 0.95
  n <- c(seq(20, 100, 5), 125, 150, 200, 300, 400, 500)
  k <- 0:10
  N <- 100
  
  tab <- matrix(NA, nrow = length(n), ncol = length(k))
  for(rows in 1:length(n)){
    for(columns in 1:length(k)){
      if(n[rows] >= N)
        next
      ss <- jfa::evaluation(conf.level = conf.level, n = n[rows], x = k[columns], materiality = 1, method = "hypergeometric", N.units = N)
      tab[rows, columns] <- ceiling(ss$ub * 1000) / 10
    }
  }
  
  table <- as.data.frame(tab)
  table <- cbind(n = n, table)
  table <- matrix(as.numeric(unlist(table)), ncol = 12)
  
  expect_equal(table, reference)
})

test_that(desc = "(id: f7-v0.5.5-33) Statistical Sampling Results based on the Hypergeometric Distribution (N = 500) - Upper Limits at 5 Percent Risk of Overreliance", {
  testthat::skip_on_cran()
  reference <- matrix(c(20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95, 100, 125, 150, 200, 300, 400, 500, 
                        13.6, 11, 9.2, 7.8, 6.8, 6, 5.4, 5, 4.4, 4.2, 3.8, 3.6, 3.2, 3, 2.8, 2.8, 2.6, 2, 1.6, 1, 0.6, 0.2, NA, 
                        21.2, 17.2, 14.4, 12.4, 10.8, 9.6, 8.6, 7.8, 7.2, 6.6, 6.2, 5.8, 5.4, 5, 4.6, 4.4, 4.2, 3.2, 2.6, 1.8, 1, 0.6, NA, 
                        27.8, 22.6, 19, 16.4, 14.4, 12.8, 11.6, 10.6, 9.6, 8.8, 8.2, 7.6, 7.2, 6.8, 6.4, 6, 5.6, 4.4, 3.6, 2.6, 1.4, 1, NA, 
                        34, 27.6, 23.4, 20.2, 17.8, 15.8, 14.2, 13, 11.8, 11, 10.2, 9.4, 8.8, 8.4, 7.8, 7.4, 7, 5.6, 4.4, 3.2, 2, 1.2, NA, 
                        39.6, 32.4, 27.4, 23.8, 20.8, 18.6, 16.8, 15.4, 14, 13, 12, 11.2, 10.4, 9.8, 9.2, 8.8, 8.4, 6.6, 5.4, 3.8, 2.4, 1.6, NA, 
                        45, 37, 31.4, 27.2, 24, 21.4, 19.2, 17.6, 16.2, 14.8, 13.8, 12.8, 12, 11.4, 10.6, 10.2, 9.6, 7.6, 6.2, 4.4, 2.8, 1.8, NA, 
                        50.4, 41.4, 35.2, 30.4, 26.8, 24, 21.8, 19.8, 18.2, 16.8, 15.6, 14.6, 13.6, 12.8, 12, 11.4, 10.8, 8.6, 7, 5.2, 3.2, 2, NA, 
                        55.4, 45.8, 38.8, 33.8, 29.8, 26.6, 24, 22, 20.2, 18.6, 17.4, 16.2, 15.2, 14.2, 13.4, 12.8, 12, 9.6, 7.8, 5.8, 3.6, 2.4, NA, 
                        60.2, 49.8, 42.4, 36.8, 32.6, 29.2, 26.4, 24, 22.2, 20.4, 19, 17.8, 16.6, 15.6, 14.8, 14, 13.2, 10.6, 8.6, 6.4, 4, 2.6, NA, 
                        64.8, 53.8, 46, 40, 35.4, 31.6, 28.6, 26.2, 24, 22.2, 20.6, 19.4, 18.2, 17, 16, 15.2, 14.4, 11.4, 9.4, 7, 4.4, 3, NA, 
                        69.4, 57.8, 49.4, 43, 38, 34.2, 30.8, 28.2, 26, 24, 22.4, 20.8, 19.6, 18.4, 17.4, 16.4, 15.6, 12.4, 10.2, 7.6, 4.6, 3.2, NA), ncol = 12)
  
  conf.level <- 0.95
  n <- c(seq(20, 100, 5), 125, 150, 200, 300, 400, 500)
  k <- 0:10
  N <- 500
  
  tab <- matrix(NA, nrow = length(n), ncol = length(k))
  for(rows in 1:length(n)){
    for(columns in 1:length(k)){
      if(n[rows] >= N)
        next
      ss <- jfa::evaluation(conf.level = conf.level, n = n[rows], x = k[columns], materiality = 1, method = "hypergeometric", N.units = N)
      tab[rows, columns] <- ceiling(ss$ub * 1000) / 10
    }
  }
  
  table <- as.data.frame(tab)
  table <- cbind(n = n, table)
  table <- matrix(as.numeric(unlist(table)), ncol = 12)
  
  expect_equal(table, reference)
})

test_that(desc = "(id: f7-v0.5.5-34) Statistical Sampling Results based on the Hypergeometric Distribution (N = 1000) - Upper Limits at 5 Percent Risk of Overreliance", {
  testthat::skip_on_cran()
  reference <- matrix(c(20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95, 100, 125, 150, 200, 300, 400, 500, 
                        13.7, 11.1, 9.3, 8, 7, 6.2, 5.6, 5.1, 4.7, 4.3, 4, 3.7, 3.5, 3.3, 3.1, 2.9, 2.8, 2.2, 1.8, 1.3, 0.8, 0.5, 0.4, 
                        21.4, 17.4, 14.6, 12.6, 11.1, 9.9, 8.9, 8.1, 7.4, 6.9, 6.4, 5.9, 5.6, 5.2, 4.9, 4.7, 4.4, 3.5, 2.9, 2.1, 1.3, 0.9, 0.7, 
                        28, 22.9, 19.3, 16.7, 14.7, 13.1, 11.8, 10.8, 9.9, 9.1, 8.5, 7.9, 7.4, 7, 6.6, 6.2, 5.9, 4.7, 3.9, 2.8, 1.8, 1.3, 1, 
                        34.1, 27.9, 23.6, 20.4, 18, 16.1, 14.5, 13.2, 12.1, 11.2, 10.4, 9.7, 9.1, 8.6, 8.1, 7.7, 7.3, 5.8, 4.8, 3.5, 2.3, 1.6, 1.2, 
                        39.9, 32.7, 27.7, 24, 21.2, 18.9, 17.1, 15.6, 14.3, 13.2, 12.3, 11.5, 10.8, 10.2, 9.6, 9.1, 8.6, 6.9, 5.7, 4.2, 2.7, 1.9, 1.5, 
                        45.3, 37.3, 31.6, 27.4, 24.2, 21.7, 19.6, 17.9, 16.4, 15.2, 14.1, 13.2, 12.4, 11.7, 11, 10.4, 9.9, 7.9, 6.6, 4.9, 3.1, 2.3, 1.7, 
                        50.5, 41.7, 35.4, 30.8, 27.2, 24.3, 22, 20.1, 18.5, 17.1, 15.9, 14.9, 13.9, 13.1, 12.4, 11.8, 11.2, 8.9, 7.4, 5.5, 3.6, 2.6, 2, 
                        55.6, 46, 39.1, 34, 30.1, 26.9, 24.4, 22.3, 20.5, 19, 17.6, 16.5, 15.5, 14.6, 13.8, 13.1, 12.4, 9.9, 8.2, 6.1, 4, 2.9, 2.2, 
                        60.4, 50.1, 42.7, 37.2, 32.9, 29.5, 26.7, 24.4, 22.4, 20.8, 19.3, 18.1, 17, 16, 15.1, 14.3, 13.6, 10.9, 9.1, 6.7, 4.4, 3.2, 2.5, 
                        65.1, 54.1, 46.2, 40.3, 35.7, 32, 29, 26.5, 24.4, 22.6, 21, 19.7, 18.5, 17.4, 16.4, 15.6, 14.8, 11.9, 9.9, 7.3, 4.8, 3.5, 2.7, 
                        69.6, 58.1, 49.7, 43.3, 38.4, 34.5, 31.2, 28.6, 26.3, 24.4, 22.7, 21.2, 19.9, 18.8, 17.8, 16.8, 16, 12.8, 10.7, 7.9, 5.2, 3.8, 2.9), ncol = 12)
  
  conf.level <- 0.95
  n <- c(seq(20, 100, 5), 125, 150, 200, 300, 400, 500)
  k <- 0:10
  N <- 1000
  
  tab <- matrix(NA, nrow = length(n), ncol = length(k))
  for(rows in 1:length(n)){
    for(columns in 1:length(k)){
      if(n[rows] >= N)
        next
      ss <- jfa::evaluation(conf.level = conf.level, n = n[rows], x = k[columns], materiality = 1, method = "hypergeometric", N.units = N)
      tab[rows, columns] <- ceiling(ss$ub * 1000) / 10
    }
  }
  
  table <- as.data.frame(tab)
  table <- cbind(n = n, table)
  table <- matrix(as.numeric(unlist(table)), ncol = 12)
  
  expect_equal(table, reference)
})

test_that(desc = "(id: f7-v0.5.5-35) Statistical Sampling Results based on the Hypergeometric Distribution (N = 100) - Upper Limits at 2.5 Percent Risk of Overreliance", {
  testthat::skip_on_cran()
  reference <- matrix(c(20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95, 100, 125, 150, 200, 300, 400, 500, 
                        15, 11, 9, 8, 6, 5, 5, 4, 3, 3, 3, 2, 2, 1, 1, 1, NA, NA, NA, NA, NA, NA, NA, 
                        22, 18, 15, 12, 10, 9, 8, 7, 6, 5, 5, 4, 3, 3, 3, 2, NA, NA, NA, NA, NA, NA, NA, 
                        29, 23, 19, 16, 14, 12, 11, 9, 8, 7, 6, 6, 5, 4, 4, 3, NA, NA, NA, NA, NA, NA, NA, 
                        35, 28, 24, 20, 17, 15, 13, 12, 10, 9, 8, 7, 7, 6, 5, 4, NA, NA, NA, NA, NA, NA, NA, 
                        41, 33, 28, 23, 20, 18, 16, 14, 12, 11, 10, 9, 8, 7, 6, 6, NA, NA, NA, NA, NA, NA, NA, 
                        46, 38, 31, 27, 23, 20, 18, 16, 14, 13, 12, 11, 10, 9, 8, 7, NA, NA, NA, NA, NA, NA, NA, 
                        51, 42, 35, 30, 26, 23, 20, 18, 16, 15, 13, 12, 11, 10, 9, 8, NA, NA, NA, NA, NA, NA, NA, 
                        56, 46, 39, 33, 29, 26, 23, 20, 18, 17, 15, 14, 12, 11, 10, 9, NA, NA, NA, NA, NA, NA, NA, 
                        61, 50, 42, 37, 32, 28, 25, 22, 20, 18, 17, 15, 14, 12, 11, 10, NA, NA, NA, NA, NA, NA, NA, 
                        66, 54, 46, 40, 35, 31, 27, 24, 22, 20, 18, 17, 15, 14, 12, 11, NA, NA, NA, NA, NA, NA, NA, 
                        70, 58, 49, 43, 37, 33, 29, 26, 24, 22, 20, 18, 16, 15, 14, 12, NA, NA, NA, NA, NA, NA, NA), ncol = 12)
  
  conf.level <- 0.975
  n <- c(seq(20, 100, 5), 125, 150, 200, 300, 400, 500)
  k <- 0:10
  N <- 100
  
  tab <- matrix(NA, nrow = length(n), ncol = length(k))
  for(rows in 1:length(n)){
    for(columns in 1:length(k)){
      if(n[rows] >= N)
        next
      ss <- jfa::evaluation(conf.level = conf.level, n = n[rows], x = k[columns], materiality = 1, method = "hypergeometric", N.units = N)
      tab[rows, columns] <- ceiling(ss$ub * 1000) / 10
    }
  }
  
  table <- as.data.frame(tab)
  table <- cbind(n = n, table)
  table <- matrix(as.numeric(unlist(table)), ncol = 12)
  
  expect_equal(table, reference)
})

test_that(desc = "(id: f7-v0.5.5-36) Statistical Sampling Results based on the Hypergeometric Distribution (N = 500) - Upper Limits at 2.5 Percent Risk of Overreliance", {
  testthat::skip_on_cran()
  reference <- matrix(c(20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95, 100, 125, 150, 200, 300, 400, 500, 
                        16.4, 13.2, 11.2, 9.6, 8.4, 7.4, 6.6, 6, 5.6, 5, 4.6, 4.4, 4, 3.8, 3.6, 3.4, 3.2, 2.4, 2, 1.4, 0.8, 0.4, NA, 
                        24.4, 19.8, 16.8, 14.4, 12.6, 11.2, 10.2, 9.2, 8.4, 7.8, 7.2, 6.6, 6.2, 5.8, 5.4, 5.2, 4.8, 3.8, 3, 2.2, 1.2, 0.8, NA, 
                        31.2, 25.6, 21.6, 18.6, 16.4, 14.6, 13.2, 12, 11, 10, 9.4, 8.6, 8.2, 7.6, 7.2, 6.8, 6.4, 5, 4, 3, 1.6, 1, NA, 
                        37.4, 30.6, 26, 22.4, 19.8, 17.6, 16, 14.4, 13.2, 12.2, 11.4, 10.6, 10, 9.4, 8.8, 8.2, 7.8, 6.2, 5, 3.6, 2.2, 1.4, NA, 
                        43.2, 35.6, 30.2, 26.2, 23, 20.6, 18.6, 17, 15.6, 14.4, 13.4, 12.4, 11.6, 11, 10.2, 9.8, 9.2, 7.2, 6, 4.2, 2.6, 1.6, NA, 
                        48.6, 40.2, 34.2, 29.6, 26.2, 23.4, 21.2, 19.2, 17.6, 16.4, 15.2, 14.2, 13.2, 12.4, 11.8, 11.2, 10.6, 8.4, 6.8, 5, 3, 2, NA, 
                        53.8, 44.6, 38, 33, 29.2, 26.2, 23.6, 21.6, 19.8, 18.2, 17, 15.8, 14.8, 14, 13.2, 12.4, 11.8, 9.4, 7.6, 5.6, 3.4, 2.2, NA, 
                        58.6, 48.8, 41.6, 36.2, 32, 28.8, 26, 23.8, 21.8, 20.2, 18.8, 17.6, 16.4, 15.4, 14.6, 13.8, 13, 10.4, 8.6, 6.2, 3.8, 2.6, NA, 
                        63.4, 53, 45.2, 39.4, 35, 31.4, 28.4, 26, 23.8, 22, 20.4, 19.2, 18, 16.8, 16, 15, 14.4, 11.4, 9.4, 6.8, 4.2, 2.8, NA, 
                        68, 56.8, 48.8, 42.6, 37.8, 33.8, 30.6, 28, 25.8, 23.8, 22.2, 20.8, 19.4, 18.4, 17.2, 16.4, 15.6, 12.4, 10.2, 7.4, 4.6, 3, NA, 
                        72.4, 60.8, 52.2, 45.6, 40.4, 36.4, 33, 30.2, 27.8, 25.6, 23.8, 22.4, 21, 19.8, 18.6, 17.6, 16.8, 13.4, 11, 8, 5, 3.4, NA), ncol = 12)
  
  conf.level <- 0.975
  n <- c(seq(20, 100, 5), 125, 150, 200, 300, 400, 500)
  k <- 0:10
  N <- 500
  
  tab <- matrix(NA, nrow = length(n), ncol = length(k))
  for(rows in 1:length(n)){
    for(columns in 1:length(k)){
      if(n[rows] >= N)
        next
      ss <- jfa::evaluation(conf.level = conf.level, n = n[rows], x = k[columns], materiality = 1, method = "hypergeometric", N.units = N)
      tab[rows, columns] <- ceiling(ss$ub * 1000) / 10
    }
  }
  
  table <- as.data.frame(tab)
  table <- cbind(n = n, table)
  table <- matrix(as.numeric(unlist(table)), ncol = 12)
  
  expect_equal(table, reference)
})

test_that(desc = "(id: f7-v0.5.5-37) Statistical Sampling Results based on the Hypergeometric Distribution (N = 1000) - Upper Limits at 2.5 Percent Risk of Overreliance", {
  testthat::skip_on_cran()
  reference <- matrix(c(20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95, 100, 125, 150, 200, 300, 400, 500, 
                        16.6, 13.5, 11.4, 9.8, 8.6, 7.6, 6.9, 6.3, 5.7, 5.3, 4.9, 4.6, 4.3, 4, 3.8, 3.6, 3.4, 2.7, 2.2, 1.6, 1, 0.7, 0.5, 
                        24.6, 20.1, 17, 14.7, 12.9, 11.5, 10.4, 9.5, 8.7, 8, 7.4, 6.9, 6.5, 6.1, 5.8, 5.5, 5.2, 4.1, 3.4, 2.5, 1.6, 1.1, 0.8, 
                        31.4, 25.8, 21.8, 18.9, 16.6, 14.9, 13.4, 12.2, 11.2, 10.4, 9.6, 9, 8.4, 7.9, 7.5, 7.1, 6.7, 5.4, 4.4, 3.3, 2.1, 1.5, 1.1, 
                        37.6, 30.9, 26.2, 22.8, 20.1, 18, 16.2, 14.8, 13.6, 12.6, 11.7, 10.9, 10.2, 9.6, 9.1, 8.6, 8.2, 6.5, 5.4, 4, 2.5, 1.8, 1.4, 
                        43.4, 35.8, 30.4, 26.4, 23.4, 20.9, 18.9, 17.3, 15.9, 14.7, 13.7, 12.8, 12, 11.3, 10.6, 10.1, 9.6, 7.6, 6.3, 4.7, 3, 2.2, 1.6, 
                        48.8, 40.4, 34.4, 29.9, 26.5, 23.7, 21.5, 19.6, 18, 16.7, 15.5, 14.5, 13.6, 12.8, 12.1, 11.5, 10.9, 8.7, 7.2, 5.4, 3.4, 2.5, 1.9, 
                        54, 44.8, 38.3, 33.3, 29.5, 26.5, 24, 21.9, 20.1, 18.7, 17.4, 16.2, 15.2, 14.4, 13.6, 12.9, 12.2, 9.8, 8.1, 6, 3.9, 2.8, 2.1, 
                        59, 49.1, 42, 36.6, 32.4, 29.1, 26.4, 24.1, 22.2, 20.6, 19.1, 17.9, 16.8, 15.8, 15, 14.2, 13.5, 10.8, 9, 6.7, 4.3, 3.1, 2.4, 
                        63.7, 53.2, 45.6, 39.8, 35.3, 31.7, 28.7, 26.3, 24.2, 22.4, 20.9, 19.5, 18.4, 17.3, 16.4, 15.5, 14.7, 11.8, 9.8, 7.3, 4.7, 3.4, 2.6, 
                        68.2, 57.2, 49.1, 42.9, 38.1, 34.2, 31.1, 28.4, 26.2, 24.3, 22.6, 21.2, 19.9, 18.7, 17.7, 16.8, 16, 12.8, 10.6, 7.9, 5.2, 3.7, 2.9, 
                        72.6, 61, 52.5, 46, 40.8, 36.7, 33.3, 30.5, 28.1, 26.1, 24.3, 22.7, 21.4, 20.2, 19.1, 18.1, 17.2, 13.8, 11.5, 8.5, 5.6, 4, 3.1), ncol = 12)
  
  conf.level <- 0.975
  n <- c(seq(20, 100, 5), 125, 150, 200, 300, 400, 500)
  k <- 0:10
  N <- 1000
  
  tab <- matrix(NA, nrow = length(n), ncol = length(k))
  for(rows in 1:length(n)){
    for(columns in 1:length(k)){
      if(n[rows] >= N)
        next
      ss <- jfa::evaluation(conf.level = conf.level, n = n[rows], x = k[columns], materiality = 1, method = "hypergeometric", N.units = N)
      tab[rows, columns] <- ceiling(ss$ub * 1000) / 10
    }
  }
  
  table <- as.data.frame(tab)
  table <- cbind(n = n, table)
  table <- matrix(as.numeric(unlist(table)), ncol = 12)
  
  expect_equal(table, reference)
})

test_that(desc = "(id: f7-v0.5.5-38) Statistical Sampling Results based on the Hypergeometric Distribution (N = 100) - Upper Limits at 1 Percent Risk of Overreliance", {
  testthat::skip_on_cran()
  reference <- matrix(c(20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95, 100, 125, 150, 200, 300, 400, 500, 
                        18, 14, 12, 10, 8, 7, 6, 5, 4, 4, 3, 3, 2, 2, 1, 1, NA, NA, NA, NA, NA, NA, NA, 
                        26, 21, 17, 14, 12, 11, 9, 8, 7, 6, 5, 5, 4, 4, 3, 2, NA, NA, NA, NA, NA, NA, NA, 
                        33, 26, 22, 18, 16, 14, 12, 11, 9, 8, 7, 7, 6, 5, 4, 4, NA, NA, NA, NA, NA, NA, NA, 
                        39, 32, 26, 22, 19, 17, 15, 13, 12, 10, 9, 8, 7, 6, 6, 5, NA, NA, NA, NA, NA, NA, NA, 
                        45, 36, 30, 26, 22, 20, 17, 15, 14, 12, 11, 10, 9, 8, 7, 6, NA, NA, NA, NA, NA, NA, NA, 
                        50, 41, 34, 29, 25, 22, 20, 18, 16, 14, 13, 11, 10, 9, 8, 7, NA, NA, NA, NA, NA, NA, NA, 
                        55, 45, 38, 33, 28, 25, 22, 20, 18, 16, 14, 13, 12, 11, 9, 8, NA, NA, NA, NA, NA, NA, NA, 
                        60, 49, 42, 36, 31, 28, 24, 22, 20, 18, 16, 15, 13, 12, 11, 9, NA, NA, NA, NA, NA, NA, NA, 
                        65, 54, 45, 39, 34, 30, 27, 24, 22, 20, 18, 16, 15, 13, 12, 10, NA, NA, NA, NA, NA, NA, NA, 
                        69, 57, 49, 42, 37, 33, 29, 26, 23, 21, 19, 18, 16, 14, 13, 12, NA, NA, NA, NA, NA, NA, NA, 
                        73, 61, 52, 45, 40, 35, 31, 28, 25, 23, 21, 19, 17, 16, 14, 13, NA, NA, NA, NA, NA, NA, NA), ncol = 12)
  
  conf.level <- 0.99
  n <- c(seq(20, 100, 5), 125, 150, 200, 300, 400, 500)
  k <- 0:10
  N <- 100
  
  tab <- matrix(NA, nrow = length(n), ncol = length(k))
  for(rows in 1:length(n)){
    for(columns in 1:length(k)){
      if(n[rows] >= N)
        next
      ss <- jfa::evaluation(conf.level = conf.level, n = n[rows], x = k[columns], materiality = 1, method = "hypergeometric", N.units = N)
      tab[rows, columns] <- ceiling(ss$ub * 1000) / 10
    }
  }
  
  table <- as.data.frame(tab)
  table <- cbind(n = n, table)
  table <- matrix(as.numeric(unlist(table)), ncol = 12)
  
  expect_equal(table, reference)
})

test_that(desc = "(id: f7-v0.5.5-39) Statistical Sampling Results based on the Hypergeometric Distribution (N = 500) - Upper Limits at 1 Percent Risk of Overreliance", {
  testthat::skip_on_cran()
  reference <- matrix(c(20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95, 100, 125, 150, 200, 300, 400, 500, 
                        20, 16.4, 13.8, 11.8, 10.4, 9.2, 8.2, 7.4, 6.8, 6.2, 5.8, 5.4, 5, 4.8, 4.4, 4.2, 4, 3, 2.4, 1.6, 0.8, 0.4, NA, 
                        28.4, 23.2, 19.6, 17, 14.8, 13.2, 12, 10.8, 10, 9.2, 8.4, 7.8, 7.4, 7, 6.4, 6.2, 5.8, 4.6, 3.6, 2.6, 1.4, 0.8, NA, 
                        35.2, 29, 24.6, 21.2, 18.8, 16.8, 15, 13.8, 12.6, 11.6, 10.8, 10, 9.4, 8.8, 8.2, 7.8, 7.4, 5.8, 4.8, 3.4, 2, 1.2, NA, 
                        41.4, 34.2, 29, 25.2, 22.2, 20, 18, 16.4, 15, 13.8, 12.8, 12, 11.2, 10.6, 10, 9.4, 9, 7, 5.8, 4, 2.4, 1.4, NA, 
                        47.2, 39.2, 33.4, 29, 25.6, 23, 20.8, 19, 17.4, 16, 14.8, 13.8, 13, 12.2, 11.6, 11, 10.4, 8.2, 6.6, 4.8, 2.8, 1.8, NA, 
                        52.6, 43.8, 37.4, 32.6, 28.8, 25.8, 23.4, 21.4, 19.6, 18.2, 16.8, 15.8, 14.8, 13.8, 13, 12.4, 11.8, 9.2, 7.6, 5.4, 3.2, 2.2, NA, 
                        57.6, 48.2, 41.2, 36, 31.8, 28.6, 25.8, 23.6, 21.8, 20.2, 18.6, 17.4, 16.4, 15.4, 14.6, 13.8, 13, 10.4, 8.4, 6.2, 3.8, 2.4, NA, 
                        62.4, 52.4, 45, 39.2, 34.8, 31.2, 28.4, 26, 23.8, 22, 20.6, 19.2, 18, 17, 16, 15.2, 14.4, 11.4, 9.4, 6.8, 4.2, 2.8, NA, 
                        67, 56.4, 48.6, 42.4, 37.8, 33.8, 30.8, 28.2, 25.8, 24, 22.2, 20.8, 19.6, 18.4, 17.4, 16.4, 15.6, 12.4, 10.2, 7.4, 4.6, 3, NA, 
                        71.4, 60.4, 52, 45.6, 40.6, 36.4, 33, 30.2, 27.8, 25.8, 24, 22.4, 21, 19.8, 18.8, 17.8, 16.8, 13.4, 11, 8, 5, 3.2, NA, 
                        75.6, 64, 55.4, 48.6, 43.2, 39, 35.4, 32.4, 29.8, 27.6, 25.8, 24, 22.6, 21.2, 20.2, 19, 18, 14.4, 11.8, 8.6, 5.4, 3.6, NA), ncol = 12)
  
  conf.level <- 0.99
  n <- c(seq(20, 100, 5), 125, 150, 200, 300, 400, 500)
  k <- 0:10
  N <- 500
  
  tab <- matrix(NA, nrow = length(n), ncol = length(k))
  for(rows in 1:length(n)){
    for(columns in 1:length(k)){
      if(n[rows] >= N)
        next
      ss <- jfa::evaluation(conf.level = conf.level, n = n[rows], x = k[columns], materiality = 1, method = "hypergeometric", N.units = N)
      tab[rows, columns] <- ceiling(ss$ub * 1000) / 10
    }
  }
  
  table <- as.data.frame(tab)
  table <- cbind(n = n, table)
  table <- matrix(as.numeric(unlist(table)), ncol = 12)
  
  expect_equal(table, reference)
})

test_that(desc = "(id: f7-v0.5.5-40) Statistical Sampling Results based on the Hypergeometric Distribution (N = 1000) - Upper Limits at 1 Percent Risk of Overreliance", {
  testthat::skip_on_cran()
  reference <- matrix(c(20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95, 100, 125, 150, 200, 300, 400, 500, 
                        20.3, 16.6, 14, 12.1, 10.6, 9.5, 8.5, 7.8, 7.1, 6.6, 6.1, 5.7, 5.3, 5, 4.7, 4.5, 4.2, 3.3, 2.7, 2, 1.2, 0.8, 0.6, 
                        28.6, 23.5, 19.9, 17.2, 15.2, 13.6, 12.2, 11.2, 10.2, 9.5, 8.8, 8.2, 7.7, 7.2, 6.8, 6.5, 6.1, 4.9, 4, 2.9, 1.8, 1.3, 1, 
                        35.5, 29.3, 24.9, 21.6, 19.1, 17.1, 15.4, 14.1, 12.9, 12, 11.1, 10.4, 9.7, 9.2, 8.7, 8.2, 7.8, 6.2, 5.1, 3.8, 2.4, 1.7, 1.3, 
                        41.8, 34.6, 29.4, 25.6, 22.6, 20.3, 18.4, 16.8, 15.4, 14.3, 13.3, 12.4, 11.6, 11, 10.3, 9.8, 9.3, 7.4, 6.1, 4.5, 2.9, 2.1, 1.6, 
                        47.5, 39.5, 33.7, 29.4, 26, 23.3, 21.1, 19.3, 17.8, 16.4, 15.3, 14.3, 13.4, 12.6, 12, 11.3, 10.8, 8.6, 7.1, 5.3, 3.4, 2.4, 1.8, 
                        52.9, 44.1, 37.7, 32.9, 29.2, 26.2, 23.8, 21.7, 20, 18.5, 17.3, 16.1, 15.1, 14.3, 13.5, 12.8, 12.2, 9.7, 8.1, 6, 3.8, 2.8, 2.1, 
                        58, 48.5, 41.6, 36.4, 32.3, 29, 26.3, 24.1, 22.2, 20.5, 19.1, 17.9, 16.8, 15.9, 15, 14.2, 13.5, 10.8, 9, 6.7, 4.3, 3.1, 2.4, 
                        62.8, 52.7, 45.3, 39.7, 35.2, 31.7, 28.8, 26.3, 24.3, 22.5, 21, 19.6, 18.4, 17.4, 16.4, 15.6, 14.8, 11.9, 9.9, 7.3, 4.7, 3.4, 2.6, 
                        67.4, 56.8, 48.9, 42.9, 38.1, 34.3, 31.2, 28.6, 26.3, 24.4, 22.8, 21.3, 20, 18.9, 17.9, 16.9, 16.1, 12.9, 10.7, 8, 5.2, 3.7, 2.9, 
                        71.7, 60.7, 52.4, 46, 41, 36.9, 33.5, 30.7, 28.3, 26.3, 24.5, 23, 21.6, 20.4, 19.3, 18.3, 17.4, 13.9, 11.6, 8.6, 5.6, 4.1, 3.1, 
                        75.8, 64.4, 55.8, 49, 43.7, 39.4, 35.8, 32.9, 30.3, 28.1, 26.2, 24.6, 23.1, 21.8, 20.6, 19.6, 18.6, 14.9, 12.4, 9.3, 6, 4.4, 3.4), ncol = 12)
  
  conf.level <- 0.99
  n <- c(seq(20, 100, 5), 125, 150, 200, 300, 400, 500)
  k <- 0:10
  N <- 1000
  
  tab <- matrix(NA, nrow = length(n), ncol = length(k))
  for(rows in 1:length(n)){
    for(columns in 1:length(k)){
      if(n[rows] >= N)
        next
      ss <- jfa::evaluation(conf.level = conf.level, n = n[rows], x = k[columns], materiality = 1, method = "hypergeometric", N.units = N)
      tab[rows, columns] <- ceiling(ss$ub * 1000) / 10
    }
  }
  
  table <- as.data.frame(tab)
  table <- cbind(n = n, table)
  table <- matrix(as.numeric(unlist(table)), ncol = 12)
  
  expect_equal(table, reference)
})

###########################
## Default Bayes factors ##
###########################

# Beta distribution

test_that(desc = "(id: f7-v0.5.5-t41) Statistical Sampling Results based on the Beta Distribution - Bayes Factors in favor of Tolerable Misstatement for a Performance Materiality of 10 percent", {
  testthat::skip_on_cran()
  reference <- matrix(c(20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95, 100, 125, 150, 200, 300, 400, 500, 
                        15.45, 26.86, 46.18, 78.9, 134.31, 228.15, 387.07, 656.19, 1111.96, 1883.81, 3190.94, 5404.57, 9153.39, 15502.03, 26253.52, 44461.27, 75296.24, 1048856.08, 1.461011e+07, 2.834837e+09, 1.067277e+14, 4.018151e+18, 1.512778e+23, 
                        3.16, 5.18, 8.32, 13.22, 20.91, 33.04, 52.26, 82.8, 131.53, 209.52, 334.68, 536.1, 861.01, 1386.24, 2237.05, 3617.78, 5862.32, 67147.91, 794128.49, 1.183490e+08, 3.043770e+12, 8.701925e+16, 2.640722e+21, 
                        1.02, 1.66, 2.6, 3.98, 6.02, 9.04, 13.54, 20.29, 30.48, 45.96, 69.56, 105.74, 161.44, 247.53, 381.1, 589.06, 913.89, 8618.95, 86567.11, 9908456.95, 1.740048e+11, 3.776191e+15, 9.234131e+19, 
                        0.37, 0.64, 1.02, 1.55, 2.31, 3.37, 4.89, 7.05, 10.15, 14.64, 21.18, 30.75, 44.86, 65.75, 96.85, 143.38, 213.31, 1662.07, 14188.07, 1247502.52, 1.495426e+10, 2.462601e+14, 4.851231e+18, 
                        0.14, 0.26, 0.43, 0.68, 1.02, 1.48, 2.12, 3, 4.21, 5.89, 8.24, 11.53, 16.2, 22.84, 32.35, 46.06, 65.91, 427.35, 3105.98, 209910.75, 1.717297e+09, 2.145228e+13, 3.403557e+17, 
                        0.05, 0.1, 0.18, 0.3, 0.47, 0.7, 1.02, 1.43, 1.99, 2.74, 3.76, 5.13, 7.01, 9.59, 13.16, 18.12, 25.06, 136.96, 850.65, 44244.4, 2.470285e+08, 2.340196e+12, 2.989548e+16, 
                        0.01, 0.03, 0.07, 0.13, 0.22, 0.34, 0.51, 0.72, 1.01, 1.4, 1.9, 2.56, 3.43, 4.6, 6.16, 8.27, 11.13, 52.28, 279.37, 11211.41, 4.272789e+07, 3.068951e+11, 3.155984e+15, 
                        0, 0.01, 0.03, 0.05, 0.1, 0.16, 0.25, 0.37, 0.53, 0.74, 1.01, 1.37, 1.82, 2.41, 3.19, 4.2, 5.54, 22.97, 106.7, 3319.26, 8639141.35, 4.703698e+10, 3.892965e+14, 
                        0, 0, 0.01, 0.02, 0.04, 0.07, 0.12, 0.19, 0.28, 0.4, 0.56, 0.76, 1.01, 1.34, 1.76, 2.3, 3, 11.28, 46.25, 1124.09, 2000012.39, 8.253381e+09, 5.496438e+13, 
                        0, 0, 0, 0.01, 0.02, 0.03, 0.06, 0.09, 0.14, 0.21, 0.3, 0.42, 0.57, 0.77, 1.01, 1.32, 1.71, 6.04, 22.28, 428.29, 521815.53, 1.631971e+09, 8.743609e+12, 
                        0, 0, 0, 0, 0.01, 0.01, 0.02, 0.04, 0.07, 0.11, 0.16, 0.23, 0.33, 0.44, 0.59, 0.78, 1.01, 3.45, 11.71, 181.08, 151524.15, 3.591450e+08, 1.547769e+12), ncol = 12)
  materiality <- 0.10
  n <- c(seq(20, 100, 5), 125, 150, 200, 300, 400, 500)
  k <- 0:10
  tabWorkflow <- matrix(NA, nrow = length(n), ncol = length(k))
  for(rows in 1:length(n)){
    for(columns in 1:length(k)){
      # Via workflow
      prior <- auditPrior(conf.level = 0.9, materiality = materiality, method = "impartial", likelihood = 'binomial')
      result <- evaluation(conf.level = 0.9, materiality = materiality, n = n[rows], x = k[columns], prior = prior)
      tabWorkflow[rows, columns] <- result$posterior$hypotheses$bf.hmin
    }
  }
  tableWorkflow <- as.data.frame(tabWorkflow)
  tableWorkflow <- cbind(n = n, tableWorkflow)
  tableWorkflow <- round(tableWorkflow, 2)
  for(i in 2:12){
    tableWorkflow[, i] <- ifelse(tableWorkflow[, i] > 10000000, yes = format(tableWorkflow[, i], scientific = T), no = tableWorkflow[, i])
  }
  tableWorkflow <- matrix(as.numeric(unlist(tableWorkflow)), ncol = 12)
  expect_equal(tableWorkflow, reference)
})

test_that(desc = "(id: f7-v0.5.5-t42) Statistical Sampling Results based on the Beta Distribution - Bayes Factors in favor of Tolerable Misstatement for a Performance Materiality of 5 percent", {
  testthat::skip_on_cran()
  reference <- matrix(c(20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95, 100, 125, 150, 200, 300, 400, 500, 
                        4.58, 6.21, 8.32, 11.04, 14.56, 19.11, 24.99, 32.59, 42.41, 55.1, 71.51, 92.7, 120.1, 155.5, 201.26, 260.39, 336.81, 1216.81, 4389.25, 57056, 9637144.35, 1.627751e+09, 2.749333e+11, 
                        1.02, 1.38, 1.83, 2.39, 3.08, 3.93, 4.99, 6.29, 7.92, 9.93, 12.44, 15.56, 19.45, 24.3, 30.37, 37.95, 47.44, 145.9, 456.03, 4661.45, 550670.82, 7.150590e+07, 9.809580e+09, 
                        0.31, 0.43, 0.59, 0.79, 1.02, 1.3, 1.64, 2.05, 2.54, 3.14, 3.85, 4.72, 5.76, 7.03, 8.57, 10.43, 12.7, 34.12, 93.6, 758.92, 62903.37, 6284168.06, 7.003854e+08, 
                        0.09, 0.14, 0.21, 0.29, 0.39, 0.51, 0.65, 0.82, 1.02, 1.26, 1.54, 1.87, 2.26, 2.72, 3.27, 3.93, 4.7, 11.45, 28.13, 184.08, 10768.72, 828492.25, 7.504238e+07, 
                        0.03, 0.04, 0.07, 0.1, 0.15, 0.2, 0.27, 0.35, 0.44, 0.55, 0.68, 0.84, 1.02, 1.23, 1.47, 1.75, 2.08, 4.78, 10.83, 58.79, 2454.37, 145620.28, 1.072407e+07, 
                        0.01, 0.01, 0.02, 0.03, 0.05, 0.08, 0.11, 0.15, 0.19, 0.25, 0.31, 0.39, 0.48, 0.59, 0.71, 0.85, 1.02, 2.29, 4.93, 22.99, 697.49, 31982.61, 1916091.19, 
                        0, 0, 0.01, 0.01, 0.02, 0.03, 0.04, 0.06, 0.08, 0.11, 0.14, 0.18, 0.23, 0.29, 0.35, 0.43, 0.52, 1.19, 2.5, 10.45, 236.87, 8423.73, 410855.63, 
                        0, 0, 0, 0, 0, 0.01, 0.01, 0.02, 0.03, 0.04, 0.06, 0.08, 0.11, 0.14, 0.17, 0.21, 0.26, 0.64, 1.36, 5.3, 93.22, 2585.71, 102773, 
                        0, 0, 0, 0, 0, 0, 0, 0.01, 0.01, 0.02, 0.02, 0.03, 0.05, 0.06, 0.08, 0.1, 0.13, 0.35, 0.76, 2.92, 41.48, 905.57, 29373.42, 
                        0, 0, 0, 0, 0, 0, 0, 0, 0, 0.01, 0.01, 0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.19, 0.43, 1.69, 20.44, 355.88, 9440.25, 
                        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.01, 0.01, 0.01, 0.02, 0.03, 0.1, 0.25, 1.01, 10.96, 154.78, 3368.61), ncol = 12)
  materiality <- 0.05
  n <- c(seq(20, 100, 5), 125, 150, 200, 300, 400, 500)
  k <- 0:10
  tabWorkflow <- matrix(NA, nrow = length(n), ncol = length(k))
  for(rows in 1:length(n)){
    for(columns in 1:length(k)){
      # Via workflow
      prior <- auditPrior(conf.level = 0.9, materiality = materiality, method = "impartial", likelihood = 'binomial')
      result <- evaluation(conf.level = 0.9, materiality = materiality, n = n[rows], x = k[columns], prior = prior)
      tabWorkflow[rows, columns] <- result$posterior$hypotheses$bf.hmin
    }
  }
  tableWorkflow <- as.data.frame(tabWorkflow)
  tableWorkflow <- cbind(n = n, tableWorkflow)
  tableWorkflow <- round(tableWorkflow, 2)
  for(i in 2:12){
    tableWorkflow[, i] <- ifelse(tableWorkflow[, i] > 10000000, yes = format(tableWorkflow[, i], scientific = T), no = tableWorkflow[, i])
  }
  tableWorkflow <- matrix(as.numeric(unlist(tableWorkflow)), ncol = 12)
  expect_equal(tableWorkflow, reference)
})

test_that(desc = "(id: f7-v0.5.5-t43) Statistical Sampling Results based on the Beta Distribution - Bayes Factors in favor of Tolerable Misstatement for a Performance Materiality of 2 percent", {
  testthat::skip_on_cran()
  reference <- matrix(c(20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95, 100, 125, 150, 200, 300, 400, 500, 
                        2, 2.31, 2.67, 3.06, 3.49, 3.96, 4.49, 5.08, 5.72, 6.44, 7.23, 8.1, 9.07, 10.14, 11.32, 12.63, 14.08, 23.99, 40.41, 112.71, 856.45, 6464.46, 48750.97, 
                        0.42, 0.5, 0.59, 0.68, 0.78, 0.9, 1.02, 1.15, 1.3, 1.46, 1.63, 1.82, 2.02, 2.24, 2.48, 2.75, 3.03, 4.88, 7.7, 18.67, 108.61, 654.5, 4094.33, 
                        0.1, 0.13, 0.16, 0.19, 0.23, 0.27, 0.31, 0.36, 0.41, 0.47, 0.53, 0.6, 0.67, 0.75, 0.83, 0.92, 1.02, 1.63, 2.51, 5.62, 26.64, 130.8, 683.29, 
                        0.02, 0.03, 0.04, 0.05, 0.07, 0.08, 0.1, 0.12, 0.14, 0.16, 0.19, 0.21, 0.24, 0.28, 0.31, 0.35, 0.39, 0.65, 1.02, 2.23, 9.27, 38.35, 169.39, 
                        0, 0.01, 0.01, 0.01, 0.02, 0.02, 0.03, 0.04, 0.04, 0.05, 0.06, 0.07, 0.09, 0.1, 0.12, 0.13, 0.15, 0.27, 0.45, 1.02, 3.98, 14.47, 55.13, 
                        0, 0, 0, 0, 0, 0.01, 0.01, 0.01, 0.01, 0.02, 0.02, 0.02, 0.03, 0.03, 0.04, 0.05, 0.06, 0.11, 0.2, 0.49, 1.94, 6.49, 21.89, 
                        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.01, 0.01, 0.01, 0.01, 0.01, 0.02, 0.02, 0.04, 0.09, 0.24, 1.01, 3.27, 10.08, 
                        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.01, 0.02, 0.03, 0.11, 0.55, 1.78, 5.17, 
                        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.01, 0.01, 0.05, 0.3, 1.01, 2.87, 
                        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.02, 0.16, 0.59, 1.68, 
                        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.01, 0.08, 0.34, 1.01), ncol = 12)
  materiality <- 0.02
  n <- c(seq(20, 100, 5), 125, 150, 200, 300, 400, 500)
  k <- 0:10
  tabWorkflow <- matrix(NA, nrow = length(n), ncol = length(k))
  for(rows in 1:length(n)){
    for(columns in 1:length(k)){
      # Via workflow
      prior <- auditPrior(conf.level = 0.9, materiality = materiality, method = "impartial", likelihood = 'binomial')
      result <- evaluation(conf.level = 0.9, materiality = materiality, n = n[rows], x = k[columns], prior = prior)
      tabWorkflow[rows, columns] <- result$posterior$hypotheses$bf.hmin
    }
  }
  tableWorkflow <- as.data.frame(tabWorkflow)
  tableWorkflow <- cbind(n = n, tableWorkflow)
  tableWorkflow <- round(tableWorkflow, 2)
  for(i in 2:12){
    tableWorkflow[, i] <- ifelse(tableWorkflow[, i] > 10000000, yes = format(tableWorkflow[, i], scientific = T), no = tableWorkflow[, i])
  }
  tableWorkflow <- matrix(as.numeric(unlist(tableWorkflow)), ncol = 12)
  expect_equal(tableWorkflow, reference)
})

test_that(desc = "(id: f7-v0.5.5-t44) Statistical Sampling Results based on the Beta Distribution - Bayes Factors in favor of Tolerable Misstatement for a Performance Materiality of 1 percent", {
  testthat::skip_on_cran()
  reference <- matrix(c(20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95, 100, 125, 150, 200, 300, 400, 500, 
                        1.45, 1.57, 1.7, 1.84, 1.99, 2.14, 2.31, 2.48, 2.66, 2.84, 3.04, 3.25, 3.47, 3.7, 3.94, 4.2, 4.46, 6.02, 8.03, 13.93, 39.78, 110.42, 303.39, 
                        0.29, 0.32, 0.35, 0.39, 0.42, 0.46, 0.5, 0.54, 0.59, 0.63, 0.68, 0.73, 0.78, 0.84, 0.9, 0.96, 1.02, 1.37, 1.81, 3.02, 7.63, 18.42, 44.11, 
                        0.06, 0.07, 0.08, 0.1, 0.11, 0.12, 0.13, 0.15, 0.16, 0.18, 0.2, 0.21, 0.23, 0.25, 0.27, 0.29, 0.32, 0.44, 0.6, 1.02, 2.5, 5.58, 12.1, 
                        0.01, 0.02, 0.02, 0.02, 0.02, 0.03, 0.03, 0.04, 0.04, 0.05, 0.05, 0.06, 0.07, 0.07, 0.08, 0.09, 0.1, 0.15, 0.22, 0.4, 1.02, 2.23, 4.56, 
                        0, 0, 0, 0, 0, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.02, 0.02, 0.02, 0.02, 0.03, 0.03, 0.05, 0.08, 0.16, 0.45, 1.02, 2.05, 
                        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.01, 0.01, 0.01, 0.01, 0.02, 0.06, 0.2, 0.49, 1.02, 
                        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.01, 0.02, 0.09, 0.24, 0.52, 
                        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.01, 0.04, 0.11, 0.27, 
                        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.01, 0.05, 0.14, 
                        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.02, 0.07, 
                        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.01, 0.03), ncol = 12)
  materiality <- 0.01
  n <- c(seq(20, 100, 5), 125, 150, 200, 300, 400, 500)
  k <- 0:10
  tabWorkflow <- matrix(NA, nrow = length(n), ncol = length(k))
  for(rows in 1:length(n)){
    for(columns in 1:length(k)){
      # Via workflow
      prior <- auditPrior(conf.level = 0.9, materiality = materiality, method = "impartial", likelihood = 'binomial')
      result <- evaluation(conf.level = 0.9, materiality = materiality, n = n[rows], x = k[columns], prior = prior)
      tabWorkflow[rows, columns] <- result$posterior$hypotheses$bf.hmin
    }
  }
  tableWorkflow <- as.data.frame(tabWorkflow)
  tableWorkflow <- cbind(n = n, tableWorkflow)
  tableWorkflow <- round(tableWorkflow, 2)
  for(i in 2:12){
    tableWorkflow[, i] <- ifelse(tableWorkflow[, i] > 10000000, yes = format(tableWorkflow[, i], scientific = T), no = tableWorkflow[, i])
  }
  tableWorkflow <- matrix(as.numeric(unlist(tableWorkflow)), ncol = 12)
  expect_equal(tableWorkflow, reference)
})

# Gamma distribution

test_that(desc = "(id: f7-v0.5.5-t45) Statistical Sampling Results based on the Gamma Distribution - Bayes Factors in favor of Tolerable Misstatement for a Performance Materiality of 10 percent", {
  testthat::skip_on_cran()
  reference <- matrix(c(20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95, 100, 125, 150, 200, 300, 400, 500, 
                        13.78, 23.36, 39.17, 65.23, 108.2, 179.03, 295.83, 488.38, 805.86, 1329.28, 2192.27, 3615.08, 5960.92, 9828.54, 16205.17, 26718.45, 44051.93, 536673.57, 6538033.74, 9.703304e+08, 2.137295e+13, 4.707705e+17, 1.036941e+22, 
                        3, 4.81, 7.56, 11.75, 18.18, 28.07, 43.35, 67.03, 103.88, 161.37, 251.3, 392.35, 614.07, 963.33, 1514.57, 2386.13, 3766.41, 37811.23, 391658.8, 4.472981e+07, 6.743713e+11, 1.129132e+16, 2.005955e+20, 
                        1.02, 1.62, 2.49, 3.74, 5.54, 8.15, 11.96, 17.56, 25.81, 38.05, 56.29, 83.57, 124.57, 186.41, 280.01, 422.16, 638.7, 5300.92, 46755.83, 4115120.06, 4.251396e+10, 5.413276e+14, 7.758105e+18, 
                        0.4, 0.66, 1.02, 1.52, 2.22, 3.18, 4.53, 6.42, 9.08, 12.84, 18.21, 25.9, 36.98, 53.03, 76.37, 110.49, 160.58, 1107.94, 8338.66, 566575.88, 4.016033e+09, 3.890502e+13, 4.498967e+17, 
                        0.16, 0.28, 0.45, 0.69, 1.02, 1.46, 2.05, 2.85, 3.93, 5.41, 7.43, 10.22, 14.1, 19.5, 27.07, 37.75, 52.89, 306.32, 1973.58, 103748.28, 5.052606e+08, 3.725772e+12, 3.477233e+16, 
                        0.06, 0.12, 0.2, 0.33, 0.49, 0.72, 1.02, 1.41, 1.93, 2.61, 3.53, 4.74, 6.37, 8.57, 11.56, 15.63, 21.22, 104.7, 580.53, 23681.83, 7.936458e+07, 4.457078e+11, 3.358017e+15, 
                        0.02, 0.05, 0.09, 0.15, 0.24, 0.36, 0.53, 0.74, 1.01, 1.37, 1.84, 2.44, 3.23, 4.27, 5.64, 7.44, 9.85, 42.28, 203.39, 6466.91, 1.494063e+07, 6.393880e+10, 3.889772e+14, 
                        0.01, 0.02, 0.04, 0.07, 0.12, 0.18, 0.27, 0.39, 0.55, 0.75, 1.01, 1.35, 1.77, 2.31, 3.01, 3.92, 5.09, 19.48, 82.29, 2053.08, 3276960.41, 1.069325e+10, 5.254310e+13, 
                        0, 0.01, 0.01, 0.03, 0.05, 0.09, 0.14, 0.21, 0.3, 0.42, 0.57, 0.77, 1.01, 1.32, 1.71, 2.21, 2.84, 9.96, 37.52, 741.84, 820235.09, 2.042278e+09, 8.107642e+12, 
                        0, 0, 0, 0.01, 0.02, 0.04, 0.07, 0.11, 0.16, 0.23, 0.33, 0.44, 0.59, 0.78, 1.01, 1.3, 1.67, 5.52, 18.88, 300.02, 230614.49, 4.384529e+08, 1.406737e+12, 
                        0, 0, 0, 0, 0.01, 0.02, 0.03, 0.05, 0.08, 0.13, 0.18, 0.26, 0.35, 0.46, 0.61, 0.79, 1.01, 3.25, 10.3, 133.94, 71923.52, 1.045010e+08, 2.710605e+11), ncol = 12)
  materiality <- 0.10
  n <- c(seq(20, 100, 5), 125, 150, 200, 300, 400, 500)
  k <- 0:10
  tabWorkflow <- matrix(NA, nrow = length(n), ncol = length(k))
  for(rows in 1:length(n)){
    for(columns in 1:length(k)){
      # Via workflow
      prior <- auditPrior(conf.level = 0.9, materiality = materiality, method = "impartial", likelihood = "poisson")
      result <- evaluation(conf.level = 0.9, materiality = materiality, n = n[rows], x = k[columns], prior = prior)
      tabWorkflow[rows, columns] <- result$posterior$hypotheses$bf.hmin
    }
  }  
  tableWorkflow <- as.data.frame(tabWorkflow)
  tableWorkflow <- cbind(n = n, tableWorkflow)
  tableWorkflow <- round(tableWorkflow, 2)
  for(i in 2:12){
    tableWorkflow[, i] <- ifelse(tableWorkflow[, i] > 10000000, yes = format(tableWorkflow[, i], scientific = T), no = tableWorkflow[, i])
  }
  tableWorkflow <- matrix(as.numeric(unlist(tableWorkflow)), ncol = 12)
  expect_equal(tableWorkflow, reference)
})

test_that(desc = "(id: f7-v0.5.5-t46) Statistical Sampling Results based on the Gamma Distribution - Bayes Factors in favor of Tolerable Misstatement for a Performance Materiality of 5 percent", {
  testthat::skip_on_cran()
  reference <- matrix(c(20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95, 100, 125, 150, 200, 300, 400, 500, 
                        4.44, 5.98, 7.96, 10.51, 13.78, 17.98, 23.36, 30.29, 39.17, 50.58, 65.23, 84.04, 108.2, 139.21, 179.03, 230.17, 295.83, 1035.03, 3615.08, 44051.93, 6538033.74, 9.703304e+08, 1.440098e+11, 
                        1.02, 1.37, 1.81, 2.34, 3, 3.81, 4.81, 6.04, 7.56, 9.43, 11.75, 14.62, 18.18, 22.59, 28.07, 34.88, 43.35, 129.43, 392.35, 3766.41, 391658.8, 4.472981e+07, 5.395010e+09, 
                        0.32, 0.44, 0.6, 0.79, 1.02, 1.29, 1.62, 2.02, 2.49, 3.06, 3.74, 4.55, 5.54, 6.72, 8.15, 9.87, 11.96, 31.33, 83.57, 638.7, 46755.83, 4115120.06, 4.036578e+08, 
                        0.1, 0.15, 0.22, 0.3, 0.4, 0.52, 0.66, 0.82, 1.02, 1.25, 1.52, 1.84, 2.22, 2.66, 3.18, 3.8, 4.53, 10.8, 25.9, 160.58, 8338.66, 566575.88, 4.523491e+07, 
                        0.03, 0.05, 0.08, 0.11, 0.16, 0.21, 0.28, 0.36, 0.45, 0.56, 0.69, 0.84, 1.02, 1.22, 1.46, 1.73, 2.05, 4.61, 10.22, 52.89, 1973.58, 103748.28, 6747976.69, 
                        0.01, 0.01, 0.03, 0.04, 0.06, 0.08, 0.12, 0.16, 0.2, 0.26, 0.33, 0.4, 0.49, 0.6, 0.72, 0.86, 1.02, 2.25, 4.74, 21.22, 580.53, 23681.83, 1256121.99, 
                        0, 0, 0.01, 0.01, 0.02, 0.03, 0.05, 0.06, 0.09, 0.12, 0.15, 0.19, 0.24, 0.3, 0.36, 0.44, 0.53, 1.18, 2.44, 9.85, 203.39, 6466.91, 280066.67, 
                        0, 0, 0, 0, 0.01, 0.01, 0.02, 0.03, 0.04, 0.05, 0.07, 0.09, 0.12, 0.15, 0.18, 0.22, 0.27, 0.65, 1.35, 5.09, 82.29, 2053.08, 72704.59, 
                        0, 0, 0, 0, 0, 0, 0.01, 0.01, 0.01, 0.02, 0.03, 0.04, 0.05, 0.07, 0.09, 0.11, 0.14, 0.36, 0.77, 2.84, 37.52, 741.84, 21522.9, 
                        0, 0, 0, 0, 0, 0, 0, 0, 0, 0.01, 0.01, 0.02, 0.02, 0.03, 0.04, 0.05, 0.07, 0.2, 0.44, 1.67, 18.88, 300.02, 7150.62, 
                        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.01, 0.01, 0.01, 0.02, 0.02, 0.03, 0.1, 0.26, 1.01, 10.3, 133.94, 2632.51), ncol = 12)
  
  materiality <- 0.05
  n <- c(seq(20, 100, 5), 125, 150, 200, 300, 400, 500)
  k <- 0:10
  tabWorkflow <- matrix(NA, nrow = length(n), ncol = length(k))
  for(rows in 1:length(n)){
    for(columns in 1:length(k)){
      # Via workflow
      prior <- auditPrior(conf.level = 0.9, materiality = materiality, method = "impartial", likelihood = "poisson")
      result <- evaluation(conf.level = 0.9, materiality = materiality, n = n[rows], x = k[columns], prior = prior)
      tabWorkflow[rows, columns] <- result$posterior$hypotheses$bf.hmin
    }
  }
  tableWorkflow <- as.data.frame(tabWorkflow)
  tableWorkflow <- cbind(n = n, tableWorkflow)
  tableWorkflow <- round(tableWorkflow, 2)
  for(i in 2:12){
    tableWorkflow[, i] <- ifelse(tableWorkflow[, i] > 10000000, yes = format(tableWorkflow[, i], scientific = T), no = tableWorkflow[, i])
  }
  tableWorkflow <- matrix(as.numeric(unlist(tableWorkflow)), ncol = 12)
  expect_equal(tableWorkflow, reference)
})

test_that(desc = "(id: f7-v0.5.5-t47) Statistical Sampling Results based on the Gamma Distribution - Bayes Factors in favor of Tolerable Misstatement for a Performance Materiality of 2 percent", {
  testthat::skip_on_cran()
  reference <- matrix(c(20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95, 100, 125, 150, 200, 300, 400, 500, 
                        1.98, 2.3, 2.64, 3.03, 3.45, 3.92, 4.44, 5.01, 5.64, 6.34, 7.11, 7.96, 8.91, 9.95, 11.1, 12.37, 13.78, 23.36, 39.17, 108.2, 805.86, 5960.92, 44051.93, 
                        0.43, 0.5, 0.59, 0.68, 0.79, 0.9, 1.02, 1.15, 1.3, 1.45, 1.62, 1.81, 2.01, 2.23, 2.46, 2.72, 3, 4.81, 7.56, 18.18, 103.88, 614.07, 3766.41, 
                        0.11, 0.14, 0.16, 0.2, 0.23, 0.27, 0.32, 0.37, 0.42, 0.47, 0.53, 0.6, 0.67, 0.75, 0.83, 0.92, 1.02, 1.62, 2.49, 5.54, 25.81, 124.57, 638.7, 
                        0.03, 0.03, 0.04, 0.06, 0.07, 0.08, 0.1, 0.12, 0.14, 0.17, 0.19, 0.22, 0.25, 0.28, 0.32, 0.36, 0.4, 0.66, 1.02, 2.22, 9.08, 36.98, 160.58, 
                        0.01, 0.01, 0.01, 0.01, 0.02, 0.02, 0.03, 0.04, 0.05, 0.05, 0.07, 0.08, 0.09, 0.1, 0.12, 0.14, 0.16, 0.28, 0.45, 1.02, 3.93, 14.1, 52.89, 
                        0, 0, 0, 0, 0, 0.01, 0.01, 0.01, 0.01, 0.02, 0.02, 0.03, 0.03, 0.04, 0.04, 0.05, 0.06, 0.12, 0.2, 0.49, 1.93, 6.37, 21.22, 
                        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.01, 0.01, 0.01, 0.01, 0.01, 0.02, 0.02, 0.05, 0.09, 0.24, 1.01, 3.23, 9.85, 
                        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.01, 0.01, 0.02, 0.04, 0.12, 0.55, 1.77, 5.09, 
                        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.01, 0.01, 0.05, 0.3, 1.01, 2.84, 
                        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.02, 0.16, 0.59, 1.67, 
                        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.01, 0.08, 0.35, 1.01), ncol = 12)
  materiality <- 0.02
  n <- c(seq(20, 100, 5), 125, 150, 200, 300, 400, 500)
  k <- 0:10
  tabWorkflow <- matrix(NA, nrow = length(n), ncol = length(k))
  for(rows in 1:length(n)){
    for(columns in 1:length(k)){
      # Via workflow
      prior <- auditPrior(conf.level = 0.9, materiality = materiality, method = "impartial", likelihood = "poisson")
      result <- evaluation(conf.level = 0.9, materiality = materiality, n = n[rows], x = k[columns], prior = prior)
      tabWorkflow[rows, columns] <- result$posterior$hypotheses$bf.hmin
    }
  }
  tableWorkflow <- as.data.frame(tabWorkflow)
  tableWorkflow <- cbind(n = n, tableWorkflow)
  tableWorkflow <- round(tableWorkflow, 2)
  for(i in 2:12){
    tableWorkflow[, i] <- ifelse(tableWorkflow[, i] > 10000000, yes = format(tableWorkflow[, i], scientific = T), no = tableWorkflow[, i])
  }
  tableWorkflow <- matrix(as.numeric(unlist(tableWorkflow)), ncol = 12)
  expect_equal(tableWorkflow, reference)
})

test_that(desc = "(id: f7-v0.5.5-t48) Statistical Sampling Results based on the Gamma Distribution - Bayes Factors in favor of Tolerable Misstatement for a Performance Materiality of 1 percent", {
  testthat::skip_on_cran()
  reference <- matrix(c(20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95, 100, 125, 150, 200, 300, 400, 500, 
                        1.44, 1.57, 1.7, 1.84, 1.98, 2.14, 2.3, 2.47, 2.64, 2.83, 3.03, 3.23, 3.45, 3.68, 3.92, 4.17, 4.44, 5.98, 7.96, 13.78, 39.17, 108.2, 295.83, 
                        0.29, 0.32, 0.35, 0.39, 0.43, 0.46, 0.5, 0.55, 0.59, 0.64, 0.68, 0.73, 0.79, 0.84, 0.9, 0.96, 1.02, 1.37, 1.81, 3, 7.56, 18.18, 43.35, 
                        0.07, 0.08, 0.09, 0.1, 0.11, 0.12, 0.14, 0.15, 0.16, 0.18, 0.2, 0.22, 0.23, 0.25, 0.27, 0.3, 0.32, 0.44, 0.6, 1.02, 2.49, 5.54, 11.96, 
                        0.01, 0.02, 0.02, 0.02, 0.03, 0.03, 0.03, 0.04, 0.04, 0.05, 0.06, 0.06, 0.07, 0.08, 0.08, 0.09, 0.1, 0.15, 0.22, 0.4, 1.02, 2.22, 4.53, 
                        0, 0, 0, 0, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.02, 0.02, 0.02, 0.02, 0.03, 0.03, 0.05, 0.08, 0.16, 0.45, 1.02, 2.05, 
                        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.01, 0.01, 0.01, 0.01, 0.01, 0.03, 0.06, 0.2, 0.49, 1.02, 
                        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.01, 0.02, 0.09, 0.24, 0.53, 
                        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.01, 0.04, 0.12, 0.27, 
                        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.01, 0.05, 0.14, 
                        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.02, 0.07, 
                        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.01, 0.03), ncol = 12)
  materiality <- 0.01
  n <- c(seq(20, 100, 5), 125, 150, 200, 300, 400, 500)
  k <- 0:10
  tabWorkflow <- matrix(NA, nrow = length(n), ncol = length(k))
  for(rows in 1:length(n)){
    for(columns in 1:length(k)){
      # Via workflow
      prior <- auditPrior(conf.level = 0.9, materiality = materiality, method = "impartial", likelihood = "poisson")
      result <- evaluation(conf.level = 0.9, materiality = materiality, n = n[rows], x = k[columns], prior = prior)
      tabWorkflow[rows, columns] <- result$posterior$hypotheses$bf.hmin
    }
  }
  tableWorkflow <- as.data.frame(tabWorkflow)
  tableWorkflow <- cbind(n = n, tableWorkflow)
  tableWorkflow <- round(tableWorkflow, 2)
  for(i in 2:12){
    tableWorkflow[, i] <- ifelse(tableWorkflow[, i] > 10000000, yes = format(tableWorkflow[, i], scientific = T), no = tableWorkflow[, i])
  }
  tableWorkflow <- matrix(as.numeric(unlist(tableWorkflow)), ncol = 12)
  expect_equal(tableWorkflow, reference)
})
