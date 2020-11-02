context("3. Comparison to AuditSampler package")

# Sample size tables can be retrieved from: https://cplusglobal.wordpress.com/2015/11/13/attributes-sample-size-using-the-hypergeometric-distribution/

test_that(desc = "(id: f3-v0.4.0-t1) Test Sample sizes for Hypergeometric distribution", {
	populationSize <- c(rep(500, 12), rep(5000, 3), 15000, 36000)
	expectedErrorRate <- c(rep(1, 3), 2, rep(1, 3), 2, 3, 4, 5, 6, rep(1, 5)) / 100
	tolerableErrorRate <- c(8, 6, 4, 5, 8, 6, 4, 5, 6, 7, 8, 9, 8, 6, 4, 6, 4) / 100
	confidenceLevel <- c(rep(90, 4), rep(95, 13)) / 100
	sampleSizeMatrix <- matrix(NA, nrow = length(populationSize), ncol = 7)
	sampleSizeMatrix[, 1] <- populationSize
	sampleSizeMatrix[, 2] <- expectedErrorRate
	sampleSizeMatrix[, 3] <- tolerableErrorRate
	sampleSizeMatrix[, 4] <- confidenceLevel
	for(i in 1:nrow(sampleSizeMatrix)){
		jfaRes <- planning(confidence = sampleSizeMatrix[i, 4], expectedError = sampleSizeMatrix[i, 2], likelihood = "hypergeometric", N = sampleSizeMatrix[i, 1], materiality = sampleSizeMatrix[i, 3])  
		sampleSizeMatrix[i, 5] <- jfaRes[["sampleSize"]]  
		jfaRes <- planning(confidence = sampleSizeMatrix[i, 4], expectedError = sampleSizeMatrix[i, 2], likelihood = "binomial", N = sampleSizeMatrix[i, 1], materiality = sampleSizeMatrix[i, 3])  
		sampleSizeMatrix[i, 6] <- jfaRes[["sampleSize"]]  
		sampleSizeMatrix[i, 7] <- sampleSizeMatrix[i, 6] - sampleSizeMatrix[i, 5]
	}
	
	auditSamplerMatrix <- matrix(NA, nrow = length(populationSize), ncol = 7)
	auditSamplerMatrix[, 1] <- populationSize
	auditSamplerMatrix[, 2] <- expectedErrorRate
	auditSamplerMatrix[, 3] <- tolerableErrorRate
	auditSamplerMatrix[, 4] <- confidenceLevel

	auditSamplerMatrix[, 5:7] <- matrix(c(46, 48, 2,
											61, 64, 3,
											90, 96, 6,
											99, 132, 33,
											55, 58, 3,
											73, 78, 5,
											140, 156, 16,
											139, 181, 42,
											158, 195, 37,
											170, 221, 51,
											178, 240, 62,
											196, 266, 70,
											58, 58, 0,
											77, 78, 1,
											154, 156, 2,
											78, 78, 0,
											156, 156, 0),
									ncol = 3, nrow = 17, byrow = TRUE)
	
	expect_equal(sampleSizeMatrix, auditSamplerMatrix)
})
