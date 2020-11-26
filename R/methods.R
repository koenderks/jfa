.stringerBound <- function(taints, confidence, n, correction = NULL){
	mle <- sum(taints) / n
	t <- ifelse(taints < 0, yes = 0, no = taints)
	t <- ifelse(taints > 1, yes = 1, no = taints)
	t <- sort(subset(t, t > 0), decreasing = TRUE)
	bound <- 1 - (1 - confidence)^(1 / n)
	if(length(t) > 0){
		propSum <- 0
		for(i in 1:length(t)){
			propSum <- propSum + (stats::qbeta(p = confidence, shape1 = i + 1, shape2 = n - i) - stats::qbeta(p = confidence, shape1 = (i - 1) + 1, shape2 = n - (i - 1)))  * t[i]
		}
		bound <- bound + propSum
	}
	if(!is.null(correction) && correction == "meikle"){
		tmin <- sort(subset(taints, taints < 0), decreasing = FALSE)
		if(length(tmin) > 0){
			prop.sum.min  <- stats::qbeta(1 + 1, n - 1, p = confidence) * abs(tmin[1])
			if(length(tmin) > 2){
				prop.sum.min.2  <- 0
				for(i in 2:length(tmin)){
					prop.sum.min.2 <- prop.sum.min.2 + (stats::qbeta(i + 1, n - 1, p = confidence) - stats::qbeta((i-1) + 1, n - 1, p = confidence)) * abs(tmin[i])
				}
				prop.sum.min    <- prop.sum.min + prop.sum.min.2
			}
			bound             <- bound - prop.sum.min
		}
	} else if(!is.null(correction) && correction == "lta"){
			tmin <- subset(taints, taints < 0)
			if(length(tmin) > 0){
				ltaCorrection <- (1/n) * sum(abs(tmin))
				bound <- bound - ltaCorrection
			}
	} else if(!is.null(correction) && correction == "pvz"){
		taints <- ifelse(taints < 0, 0, taints)
		tmin <- sort(subset(taints, taints > 0))
		if(length(tmin) > 0){
			constant <- 0
			for(i in 1:length(tmin)){
				constant <- constant + (((n - 2 * i + 1)/(2 * sqrt(i*(n - i + 1)))) * rev(tmin)[i])
			}
			constant <- (1/n) * constant
			sigma <- (1/n) * sum((tmin - mean(tmin))^2)
			pvzCorrection <- (constant - sqrt(sigma)) / sqrt(n) * stats::qnorm(p = confidence, lower.tail = TRUE)
			bound <- bound - pvzCorrection
		}
	}
	result 					<- list()
	result[["confBound"]] 	<- bound
	result[["mle"]] 		<- mle
	result[["precision"]] 	<- result[["confBound"]] - result[["mle"]]
	return(result)
}

.rohrbachBound <- function(taints, confidence, n, N = NULL, rohrbachDelta){
	if(is.null(N))
		stop("Rohrbach's bound requires that you specify the population size N")
	w 						<- 1 - taints
	mu 						<- mean(taints)
	vars 					<- sum(w^2)/n - (2-(rohrbachDelta/n)) * ((1/2) * ((sum(w^2)/n) - stats::var(w)))
	result 					<- list()
	result[["confBound"]] 	<- mu + stats::qnorm(p = confidence) * sqrt((1-(n/N)) * (vars/n))
	result[["mle"]] 		<- sum(taints) / n
	result[["precision"]] 	<- result[["confBound"]] - result[["mle"]]
	return(result)
}

.momentBound <- function(taints, confidence, n, momentPoptype){
	if(!(momentPoptype %in% c("inventory", "accounts")))
		stop("Specify a valid population type. Either inventory or accounts.")
	tall <- subset(taints, taints != 0)
	if(momentPoptype == "inventory" & length(tall) > 0){
		tstar <- 0.81 * (1 - 0.667 * tanh(10 * abs(mean(tall))))
	} else if(momentPoptype == "inventory" & length(tall) == 0){
		tstar <- 0.81 * (1 - 0.667 * tanh(10 * 0))
	}
	if(momentPoptype == "accounts" & length(tall) > 0){
		tstar <- 0.81 * (1 - 0.667 * tanh(10 * mean(tall))) * (1 + 0.667 * tanh(length(tall) / 10))
	} else if(momentPoptype == "accounts" & length(tall) == 0){
		tstar <- 0.81 * (1 - 0.667 * tanh(10 * 0)) * (1 + 0.667 * tanh(0 / 10))
	}
	ncm1_z <- (tstar^1 + sum(tall^1)) / (length(tall) + 1)
	ncm2_z <- (tstar^2 + sum(tall^2)) / (length(tall) + 1)
	ncm3_z <- (tstar^3 + sum(tall^3)) / (length(tall) + 1)
	ncm1_e <- (length(tall) + 1) / (n + 2)
	ncm2_e <- ((length(tall) + 2) / (n + 3)) * ncm1_e
	ncm3_e <- ((length(tall) + 3) / (n + 4)) * ncm2_e
	ncm1_t <- ncm1_e * ncm1_z
	ncm2_t <- (ncm1_e * ncm2_z + ((n - 1) * ncm2_e * ncm1_z^2)) / n
	ncm3_t <- ((ncm1_e * ncm3_z + (3 * (n - 1) * ncm2_e * ncm1_z * ncm2_z)) / n^2) + (((n - 1) * (n - 2) * ncm3_e * ncm1_z^3)/(n^2))
	cm2_t  <- ncm2_t - ncm1_t^2
	cm3_t  <- ncm3_t - (3 * ncm1_t * ncm2_t) + (2 * ncm1_t^3)
	A      <- (4 * cm2_t^3)/(cm3_t^2)
	B      <- cm3_t / (2 * cm2_t)
	G      <- ncm1_t - ((2 * cm2_t^2)/cm3_t)
	result 					<- list()
	result[["confBound"]] 	<- G + (A * B * (1 + (stats::qnorm(confidence, mean = 0, sd = 1)/ sqrt(9 * A)) - (1 / (9 * A)))^3)
	result[["mle"]] 		<- sum(taints) / n
	result[["precision"]] 	<- result[["confBound"]] - result[["mle"]]
	return(result)
}

.coxAndSnellBound <- function(taints, confidence, n, csA = 1, csB = 3, csMu = 0.5, aPrior = 1, bPrior = 1){
	piPrior <- aPrior / (aPrior + bPrior)
	taints 	<- subset(taints, taints > 0)
	M 		<- length(taints)
	t_bar 	<- mean(taints)
	if(M == 0)
		t_bar <- 0
	result 								<- list()
	result[["multiplicationFactor"]] 	<- ((M + csA) / (M + csB)) * ((csMu * (csB - 1)) + (M * t_bar)) / (n + (csA / piPrior))
	result[["df1"]] 					<-  2 * (M + csA)
	result[["df2"]] 					<- 2 * (M + csB)
	result[["confBound"]] 				<- result[["multiplicationFactor"]] * stats::qf(p = confidence, df1 = result[["df1"]], df2 = result[["df2"]])
	result[["mle"]] 					<- result[["multiplicationFactor"]] * (((result[["df1"]]-2)/result[["df1"]]) * (result[["df2"]] / (result[["df2"]] + 2)))
	result[["precision"]] 				<- result[["confBound"]] - result[["mle"]]
	return(result)
}

.mpuMethod <- function(taints, confidence, n){
	result 					<- list()
	result[["confBound"]] 	<- mean(taints) + stats::qnorm(p = confidence) * (stats::sd(taints) / sqrt(n))
	result[["mle"]] 		<- sum(taints) / n
	result[["precision"]] 	<- result[["confBound"]] - result[["mle"]]
	return(result)	
}

.directMethod <- function(bookValues, auditValues, confidence, N = NULL, n, populationBookValue = NULL, correction = FALSE){
	if(is.null(N))
		stop("The direct method requires that you specify the population size N")
	if(is.null(populationBookValue))
		stop("The direct method requires that you specify the total population book value")
	w 								<- mean(auditValues)
	s 								<- stats::sd(auditValues)
	tVal 							<- stats::qt(p = confidence + ((1 - confidence) / 2), df = n - 1)
	errorMargin 					<- tVal * (s / sqrt(n))
	result <- list()
	result[["pointEstimate"]] 		<- populationBookValue - N * w
	if(correction){
		result[["lowerBound"]] 		<- populationBookValue - N * (w + errorMargin * sqrt((N-n)/(N-1)))
		result[["upperBound"]] 		<- populationBookValue - N * (w - errorMargin * sqrt((N-n)/(N-1)))
	} else {
		result[["lowerBound"]] 		<- populationBookValue - N * (w + errorMargin)
		result[["upperBound"]] 		<- populationBookValue - N * (w - errorMargin)
	}
	result[["precision"]] 			<- N * errorMargin
	return(result)
}

.differenceMethod <- function(bookValues, auditValues, confidence, N = NULL, n, populationBookValue = NULL, correction = FALSE){
	if(is.null(N))
		stop("The difference method requires that you specify the population size N")
	if(is.null(populationBookValue))
		stop("The difference method requires that you specify the total population book value")
	we 								<- mean(bookValues - auditValues)
	s 								<- stats::sd(bookValues - auditValues)
	tVal 							<- stats::qt(p = confidence + ((1 - confidence) / 2), df = n - 1)
	errorMargin 					<- tVal * (s / sqrt(n))
	result 							<- list()
	result[["pointEstimate"]] 		<- N * we
	if(correction){
		result[["lowerBound"]] 		<- N * (we - errorMargin * sqrt((N-n)/(N-1)))
		result[["upperBound"]] 		<- N * (we + errorMargin * sqrt((N-n)/(N-1)))
	} else {
		result[["lowerBound"]] 		<- N * (we - errorMargin)
		result[["upperBound"]] 		<- N * (we + errorMargin)
	}
	result[["precision"]] 			<- N * errorMargin
	return(result)
}

.quotientMethod <- function(bookValues, auditValues, confidence, N = NULL, n, populationBookValue = NULL, correction = FALSE){
	if(is.null(N))
		stop("The quotient method requires that you specify the population size N")
	if(is.null(populationBookValue))
		stop("The quotient method requires that you specify the total population book value")
	w 								<- mean(auditValues)
	sw 								<- stats::sd(auditValues)
	b 								<- mean(bookValues)
	sb 								<- stats::sd(bookValues)
	r 								<- stats::cor(bookValues, auditValues)
	q 								<- w / b
	s 								<- sqrt( sw^2 - 2 * q * r * sb * sw + q^2 * sb^2 )
	tVal 							<- stats::qt(p = confidence + ((1 - confidence) / 2), df = n - 1)
	errorMargin 					<- tVal * (s / sqrt(n))
	result	 						<- list()
	result[["pointEstimate"]] 		<- N * ((1 - q) * b)
	if(correction){
		result[["lowerBound"]] 		<- N * ((1 - q) * b - errorMargin * sqrt((N-n)/(N-1)))
		result[["upperBound"]] 		<- N * ((1 - q) * b + errorMargin * sqrt((N-n)/(N-1)))
	} else {
		result[["lowerBound"]] 		<- N * ((1 - q) * b - errorMargin)
		result[["upperBound"]] 		<- N * ((1 - q) * b + errorMargin)
	}
	result[["precision"]] 			<- N * errorMargin
	return(result)
}

.regressionMethod <- function(bookValues, auditValues, confidence, N = NULL, n, populationBookValue = NULL, correction = FALSE){
	if(is.null(N))
		stop("The regression method requires that you specify the population size N")
	if(is.null(populationBookValue))
		stop("The regression method requires that you specify the total population book value")
	w 								<- mean(auditValues)
	sw 								<- stats::sd(auditValues)
	b 								<- mean(bookValues)
	r 								<- stats::cor(bookValues, auditValues)
	coefs 							<- stats::coef(stats::lm(auditValues ~ bookValues))
	b1 								<- as.numeric(coefs[2])
	s 								<- sw * sqrt(1 - r^2)
	tVal 							<- stats::qt(p = confidence + ((1 - confidence) / 2), df = n - 1)
	errorMargin 					<- tVal * (s / sqrt(n))
	result 							<- list()
	result[["pointEstimate"]] 		<- populationBookValue - (N * w + b1 * (populationBookValue - N * b))
	if(correction){
		result[["lowerBound"]] 		<- result[["pointEstimate"]] - N * errorMargin * sqrt((N-n)/(N-1))
		result[["upperBound"]] 		<- result[["pointEstimate"]] + N * errorMargin * sqrt((N-n)/(N-1))
	} else {
		result[["lowerBound"]] 		<- result[["pointEstimate"]] - N * errorMargin
		result[["upperBound"]] 		<- result[["pointEstimate"]] + N * errorMargin
	}
	result[["precision"]] 			<- N * errorMargin
	return(result)
}