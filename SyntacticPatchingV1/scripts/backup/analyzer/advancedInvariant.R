library(strucchange)
library(MASS)
rSquareAccept = 0.90 # Minimum R^2 for model to be accepted

# Arguments:
#		lm: R linear model object
# Description:
#		Extracts useful information from a linear model object
# 		Stores info into a list
# Returns:
#		out: list with names of important information
extract <- function(lm)
{
	out.names <- c("inputNames", "p", "rSquare", "model")
	out <- vector("list", length(out.names))
	names(out) <- out.names

	if(is.recursive(lm)){
		if(length(names(lm$model)) > 1){
			out$inputNames <- names(lm$model)[2:length(names(lm$model))]
		}

		# Error prevention: If for some reason r.squared was not created
		# 					catch it and set it to 0
		if(is.atomic(summary(lm))){
			out$rSquare <- 0
		} else {
			out$rSquare <- summary(lm)$r.squared
		}
		out$model <- lm$model
	} else {
		# out$rSquare <- lm$r.squared
		out$rSquare <- 1
	}

	return(out)
}

# Arguments:
#		var: column (list) of data
# Description:
#		Checks every value whether it is a 0 or 1
# Returns:
#		True if column is binary
isBinary <- function(var)
{
	for (i in 1:length(var)){
		value <- var[i]
		if ((value != 0) && (value != 1)){
			return(FALSE)
		}
	}
	return(TRUE)
}

# Arguments:
#		var: column (list) of data
# Description:
#		Checks if every column is positive
# Returns:
#		True if every data entry is positive
isPositive <- function(var)
{
	for(i in 1:(length(var))){
		value <- var[i]
		if(value < 1){
			return(FALSE)
		}
	}
	return(TRUE)
}

# Arguments:
#		var: column (list) of data
# Description:
#		Checks if entire column is equal to the first value
# Returns:
#		True if entire column is constant
isConstant <- function(var)
{
	if(length(var) > 0){
		value <- var[1]
		for (i in 1:length(var)){
			current <- var[i]
			if(value != current){
				return(FALSE)
			}
		}
		return(TRUE)
	} else {
		return(FALSE)
	}
}

# Arguments:
#		input: column (list) of data
#		output: column (list) of data
# Description:
#		Compares the data index by index
# Returns:
#		True if both columns are equal
isNotChanged <- function(input, output)
{
	for (i in 1:length(input)){
		if (input[i] != output[i]){
			return(FALSE)
		}
	}
	return(TRUE)
}

# Arguments:
#		inDF: input dataframe
#		outDF: output dataframe
# Description:
# 		Generates a list of lists with names: constant, binary, unchanged, changed
# 		Populates these lists with variable names
# Returns:
#		varList: list of lists of variable names separated by type
analyzeVars <- function(inDF)
{
	# Instantiate List with types as names
	varList.names <- c("binary", "unchanged", "changed")
	varList <- vector("list", length(varList.names))
	names(varList) <- varList.names

	if(length(names(inDF)) > 0){
		for(i in 1:length(names(inDF))){
			# Assign input & output columns for simplicity
			input <- inDF[,i]
			varName <- names(inDF)[i]

			if(isBinary(input) & !(isConstant(input))){
				varList$binary <- c(varList$binary, varName)
			} else if(isConstant(input)){
				varList$unchanged <- c(varList$unchanged, varName)
			} else {
				varList$changed <- c(varList$changed, varName)
			}
		}
	}

	return(varList)
}

makeTemplateDF <- function(inDF, currentCol)
{
	nameList <- list()
	length <- length(inDF[,1])
	slrList <- list()
	df <- list()
	for(i in 1:length(names(inDF))){
		if(names(inDF)[i] != currentCol){
			nameList <- c(nameList, list(names(inDF)[i]))
			df <- list(df, list(inDF[,i]))
		}
	}
	# Add multiplication models
	for(k in 1:(length(names(inDF))-1)) {
		for(l in (k+1):length(names(inDF))){
			nameList <- c(nameList, list(paste(names(inDF)[k], "*", names(inDF)[l])))
			df <- list(df, list(inDF[,k]*inDF[,l]))
		}
	}
	# Add division models
	for(k in 1:(length(names(inDF)))) {
		for(l in 1:length(names(inDF))){
			if(l != k){
				nameList <- c(nameList, list(paste(names(inDF)[k], "/", names(inDF)[l])))
				df <- list(df, list(inDF[,k]/inDF[,l]))
			}
		}
	}
	# Add polynomial models
	for(k in 1:(length(names(inDF))/2)) {
		nameList <- c(nameList, list(paste(names(inDF)[k], "^2", sep="")))
		df <- list(df, list(inDF[,k]*inDF[,k]))

		nameList <- c(nameList, list(paste(names(inDF)[k], "^3", sep="")))
		df <- list(df, list(inDF[,k]*inDF[k]*inDF[,k]))
	}

	df <- as.data.frame(matrix(unlist(df), nrow=length , byrow=F))
	names(df) <- nameList

	for(i in 1:length(names(df))){
		df[,i][which(!is.finite(df[,i]))] = NA
	}

	# df <- df[complete.cases(df),] # Remove rows hopefully with n/a??

	# print("Dataframe from template coming: " )
	# print(df)
	return(df)
}

# Arguments:
#		inDF: input dataframe
#		outDF: output dataframe
#		changed: list of names of changed variables
# Description:
# 		Performs simple linear regression and stepwise model selection
# 		for each changed variable from the output
# Returns:
#		slrList: list of linear models (lm objects)
simpleLinearRegression <- function(inDF, changed)
{
	slrList <- list()

	if(length(changed) > 0){
		for(i in 1:length(changed)){
			input <- makeTemplateDF(inDF, changed[i])
			output <- inDF[,changed[i]]
			slm.out <- lm( output ~ ., data=input)

			# if(!(isConstant(output))){
			#	slm.out <- step(slm.out)
			# }
			# Commented out until I figure out how to properly use the step function

			# Adds the model as long as all of the input variables were not removed
			### This may actually be an issue with constants
			info <- extract(slm.out)
			if(length(info$inputNames) > 0){
				slrList[[i]] <- slm.out
				names(slrList)[i] <- changed[i]
			}
		}
	}

	return(slrList)
}


# Arguments:
#		lmList: list of lm objects
# Description:
#		Iterates through each model and determines it's rSquare value
#		Separates models into a "good" and "bad" list if the rSquare meets the threshold
# Returns:
#		toReturn: list of models separated by $good and $bad names
sift <- function(lmList)
{
	toReturn.names <- c("good", "bad")
	toReturn <- vector("list", length(toReturn.names))
	names(toReturn) <- toReturn.names

	toReturn$good <- list()
	toReturn$bad <- list()
	nameListGood <- list()
	nameListBad <- list()

	if(length(lmList) > 0){
		for(i in 1:length(names(lmList))){
			name <- names(lmList)[i]
			model <- lmList[[i]]
			out <- extract(model)
			output <- model$model$output

			if(isConstant(output) || (out$rSquare > rSquareAccept)){
				toReturn$good <- c(toReturn$good, list(model))
				nameListGood <- c(nameListGood, name)
			} else {
				l <- length(toReturn$bad) + 1
				toReturn$bad[[l]] <- model
				nameListBad <- c(nameListBad, name)
			}
		}
	}

	names(toReturn$good) <- nameListGood
	names(toReturn$bad) <- nameListBad

	return(toReturn)
}


# Arguments:
#		segments: list of segmented data passed from segment
# Description:
#		Computes a linear regression model on each segment of data
#		Averages the collective rSquare value of each segment
# Returns:
#		test: list of generated linear models and average rSquare value
testSegments <- function(segments)
{
	test <- list()
	lmList <- list()

	rSquareTotal <- 0
	if(length(segments) > 0){
		for(i in 1:length(segments)){
			seg <- segments[[i]]
			input <- names(seg)[2:length(names(seg))]

			if(length(input) > 0){
				if(length(seg$output) > 0){
					if(length(seg[input[1]]) > 0){
						if(isConstant(seg$output)){
							coef <- seg$output[1]
							rSquareTotal <- rSquareTotal + 1
						} else {
							slm.out <- lm(seg$output ~ ., data=seg[input])
							coef <- slm.out
							rSquareTotal <- rSquareTotal + summary(slm.out)$r.squared
						}

						lmList[[i]] <- coef
						names(lmList)[i] <- names(segments)[i]
					}
				}
			}
		}
	}

	test[[1]] <- lmList
	if(length(segments) > 0){
		test[[2]] <- (rSquareTotal)/(length(segments))
	} else {
		test[[2]] <- 0
	}
	names(test) <- c("linear", "score")

	return(test)
}

# Arguments:
#		model: lm object
# Description:
#		Attempts to find breakpoints for the model using the breakpoints() function
#		If any breakpoints are produced, will segment on those breakpoints
#		Then use testSegment() to get the average rSquare for the segments
# Returns:
#		toReturn: list of data segments, corresponding linear models, and average rSquare
segment <- function(model)
{
	toReturn <- list()
	currentScore <- 0
	currentSegments <- NULL
	currentSegLinear <- NULL

	out <- extract(model)
	model <- out$model

	for(i in 1:length(names(out$inputNames))){
		segments <- list()

		column <- out$inputNames[i]

		model <- as.data.frame(model[ order(model[column]), ])
		input <- model[column]

		if(length(input) > 0){
			bp <- breakpoints(model$output ~ as.matrix(input), h=.03)$breakpoints

			# Segment at breakpoints
			if(!(is.na(bp[1]))){
				for(j in 1:(length(bp)+1)){
					if(j==1){ # First segment
						seg <- subset(model, model[column] <= model[bp[j], column])
						rangeString <- paste("X <= ", model[bp[j], column], sep="")

					} else if(j==(length(bp)+1)){ # End segment
						seg <- subset(model, model[column] > model[bp[j-1], column])
						rangeString <- paste("X > ", model[bp[j-1], column], sep="")

					} else { # Middle segments
						seg <- subset(model, model[column] > model[bp[j-1], column] & model[column] <= model[bp[j], column])
						rangeString <- paste(model[bp[j-1], column], " < X <= ", model[bp[j], column], sep="")

					}

					l = length(segments) + 1
					if(!(length(seg[[1]]) == 0)){
						segments[[l]] <- seg
						names(segments)[l] <- rangeString
					}
				}
			}
		}

		test <- testSegments(segments)
		score <- test$score
		if(score > currentScore){
			currentScore <- score
			currentSegments <- segments
			currentSegLinear <- test$linear
		}
	}


	toReturn[[1]] <- currentSegments
	toReturn[[2]] <- currentSegLinear
	toReturn[[3]] <- currentScore
	names(toReturn) <- c("segments", "linear", "r.squared")

	return(toReturn)
}

# Arguments:
#		inDF: input dataframe
#		outDF: output dataframe
#		binary: list of variable names w/ binary output
# Description:
#		For each binary variable, computes breakpoints
#		using the breakpoints() function
#		Analyzes those segments for each input variable
#		If the output for each segment is constant
#		The model will be stored in the list that is returned
# Returns:
#		analyzed: list of acceptable binary segments
analyzeBinary <- function(inDF, binary)
{
	if(length(binary) > 0){
		for(i in 1:length(binary)){
			analyzed <- list()
			name <- binary[1]
			output <- inDF[name]
			input <- subset(inDF, TRUE, select= !names(DF) %in% changed[i])

			for(j in 1:length(names(input))){
				good <- TRUE
				segments <- list()
				inCol <- names(input)[j]

				# Need to convert to use only one data frame and match the one var against all others excluding self
				# Not a focus atm
				output <- as.data.frame(output[ order(inDF[input]), ])

				bp <- breakpoints(as.matrix(output) ~ as.matrix(inDF), h=0.15)$breakpoints

				if(!(is.na(bp[1]))){
					for(k in 1:(length(bp)+1)){
						if(k==1){ # first segment
							outSeg <- subset(output, inDF[input] < inDF[bp[k], input])
							if(!(isConstant(outSeg))){
								good <- FALSE
							}
							out <- outSeg[1,1]
							rangeString <- paste("X < ", inDF[bp[k], input], sep="")

						} else if(k==(length(bp)+1)){ # end segment
							outSeg <- subset(output, inDF[input] > inDF[bp[k-1], input])
							if(!(isConstant(outSeg))){
								good <- FALSE
							}
							out <- outSeg[1,1]
							rangeString <- paste("X > ", inDF[bp[k-1], input], sep="")

						} else { #middle segments
							outSeg <- subset(output, inDF[input] < inDF[bp[k], input] & inDF[input] > inDF[bp[k-1], input])
							if(!(isConstant(outSeg))){
								good <- FALSE
							}
							out <- outSeg[1,1]
							rangeString <- paste(inDF[bp[k-1], input], " < X < ", inDF[bp[k], input], sep="")

						}

						segments[[k]] <- out
						names(segments)[k] <- rangeString
					}

					if(good){ #### Won't account for which variable it was sorted on
						loc <- length(analyzed)+1
						analyzed[[loc]] <- segments
						names(analyzed)[loc] <- name
					}
				}
			}

		}
		return(analyzed)
	}
	return(NULL)
}

# Arguments:
#		modelList: list of lm objects
# Description:
#		Iterates through each model and runs segment() on them
#		Stores segmented models into segModels
# Returns:
#		segModels: list of segmented models produced by segment()
segmentAll <- function(modelList)
{
	segModels <- list()

	if(length(modelList) > 0){
		for(i in 1:length(modelList)){
			segments <- segment(modelList[[i]])
			segModels[[i]] <- segments
			names(segModels)[i] <- names(modelList)[i]
		}
	}

	return(segModels)
}




# Arguments:
#		segmented: list of segmented models produced by segment()
#		toPrint: string that compiles strings to return at the end of analysis
# Description:
#		For each segmented model, extracts essential data
#		Adds data to toPrint
# Returns:
#		toPrint: string that compiles strings to return at the end of analysis
printSegmented <- function(segmented, toPrint)
{
	if(length(segmented) > 0){
		for(i in 1:length(segmented)){
			var <- names(segmented)[i]
			segments <- segmented[[i]]$linear
			toPrint <- paste(toPrint, var, ": ", sep="")

			for(j in 1:length(segments)){
				segModel <- segments[[j]]
				rangeString <- names(segments)[j]


				toPrint <- paste(toPrint, "|", rangeString, ": ", sep="")

				if(length(segModel) > 1){
					toPrint <- printModel(segModel, toPrint)
				} else {
					toPrint <- paste(toPrint, " = ", segModel, sep="")
				}
			}
		}
	} else {
		toPrint <- paste(toPrint, "N/A\n", sep="")
	}

	return(toPrint)
}

### TEMPORARY! Just here to have something. Will spruce up later
printBad <- function(bad, toPrint)
{
	if(length(bad) > 0){
		for(i in 1:length(bad)){
			toPrint <- paste(toPrint, names(bad)[i], '\n', sep="")
		}
	} else {
		toPrint <- paste(toPrint, 'N/A\n', sep="")
	}

	return(toPrint)
}

# Arguments:
#		model: lm object
#		toPrint: string that compiles strings to return at the end of analysis
# Description:
#		Extracts the important information from the model
#		Adds info to toPrint in string form
# Returns:
#		toPrint: string that compiles strings to return at the end of analysis
printModel <- function(model, toPrint)
{
	output <- model$model$output

	if(isConstant(output)){
		value <- output[1]
		toPrint <- paste(toPrint, " = ", value, " //RSQUARE=1", sep="")
	} else {
		for(j in 1:length(model$coefficients)){
			var <- names(model$coefficients)[j]
			value <- model$coefficients[j]
			value <- round(model$coefficients[j], digits=3)
			if(length(value) == 0 || is.na(value) || is.null(value)){
				value <- 0
			}


			if(value > 0.25 || value < -0.25 ){ # || value == 0 ){
				toPrint <- paste(toPrint, "+", value, "*", var, sep="")
			}
		}
		rsq <- extract(model)$rSquare
		toPrint <- paste(toPrint, " //RSQUARE=", rsq, sep="")
	}

	return(toPrint)
}

# Arguments:
#		modelList: list of lm objects
#		toPrint: string that compiles strings to return at the end of analysis
#		other: Default to false, a check if there is more data of the same type to be printed
# Description:
#		For each model in the list, compiles information to print
#		onto the toPrint string
#		If there are no models in the list, adds 'N/A' to toPrint
# Returns:
#		toPrint: string that compiles strings to return at the end of analysis
printModels <- function(modelList, toPrint)
{
	if(length(modelList) > 0){
		for(i in 1:length(modelList)){
			model <- modelList[[i]]
			var <- names(modelList)[i]
			toPrint <- paste(toPrint, var, "=", sep="")

			toPrint <- printModel(model, toPrint)
			toPrint <- paste(toPrint, "\n", sep="")
		}
	} else {
		toPrint <- paste(toPrint, "N/A\n", sep="")
	}

	return(toPrint)
}

# Arguments:
#		binaryList: list of binary models generated by analyzeBinary()
#		toPrint: string that compiles strings to return at the end of analysis
# Description:
#		Gathers important information from binary models
#		Adds info to the toPrint string
# Returns:
#		toPrint: string that compiles strings to return at the end of analysis
printBinary <- function(binaryList, toPrint)
{
	if(length(binaryList) > 0){
		for(i in 1:length(binaryList)){
			segments <- binaryList[[i]]
			name <- names(binaryList)[i]

			toPrint <- paste(toPrint, name, ": ", sep="")

			for(j in 1:length(segments)){
				output <- segments[[j]]
				rangeString <- names(segments)[j]

				toPrint <- paste(toPrint, rangeString, "=", output, "|", sep="")
			}

			toPrint <- paste(toPrint, "\n", sep="")
		}
	} else {
		toPrint <- paste(toPrint, "N/A\n", sep="")
	}
	return(toPrint)
}

# Arguments:
#		results: object generated by run(), holds all of the analysis data
# Description:
#		Utilizes the various print functions to print
#		the vital information to the toPrint string
# Returns:
#		toPrint: string that compiles strings to return at the end of analysis
printResults <- function(results)
{
	toPrint <- "SECTION=simple\n"
	simple <- results$good$Simple
	toPrint <- printModels(simple, toPrint)

	toPrint <- paste(toPrint, "\nSECTION=transformed\nN/A\n", sep="")
	toPrint <- paste(toPrint, "\nSECTION=binary\nN/A\n", sep="")

	toPrint <- paste(toPrint, "\nSECTION=segmented\n", sep="")
	segmented <- results$good$segmented
	toPrint <- printSegmented(segmented, toPrint)

	toPrint <- paste(toPrint, "\nSECTION=bad\n", sep="")
	toPrint <- printBad(results$bad, toPrint)

	return(toPrint)
}


# Arguments:
#		inDF: input dataframe
#		outDF: output dataframe
# Description:
#		Runs all of the analysis on the data from one function
#		Generates a list of each analysis type
# Returns:
#		toReturn: list holding all necessary information from the analysis
run <- function(inDF)
{
	acceptable <- list()
	toReturn <- list()

	varList <- analyzeVars(inDF)

	# BinaryList not the main focus
	# binaryList <- analyzeBinary(inDF, varList$binary)
	binaryList <- list()

	slrList <- simpleLinearRegression(inDF, varList$changed)
	#print(slrList)
	#print("Hello\n\n")
	temp <- sift(slrList)
	acceptable[[1]] <- temp$good
	#print(temp$good)

	## May need to modify this to remove names from changed as models are accepted
	## Nothing will probably ever be poly
	## Thus segmented will have to try every column
	segments <- segmentAll(temp$bad)
	temp <- sift(segments)
	acceptable[[5]] <- temp$good

	names(acceptable) <- c("Simple", "SimpleMultiplied", "SimpleDivided", "SimplePoly", "Segmented")

	toReturn[[1]] <- varList
	toReturn[[2]] <- acceptable
	toReturn[[3]] <- binaryList
	toReturn[[4]] <- temp$bad

	names(toReturn) <- c("variables", "good", "binary", "bad")

	return(toReturn)
}

# Arguments:
#		None
# Description:
#		Reads data from file
#		Sends data to run()
#		Uses results to print
#		prints string results to STDOUT
# Returns:
#		None
main <- function()
{
	args <- commandArgs(TRUE)
	inputsFP <- args[1]					# Input data file path

	writeTo <- args[3] 					# File to write results too

	inputsDF <- read.csv(inputsFP)		# Generate inputs dataframe

	results <- run(inputsDF)

	toPrint <- printResults(results)
	# write(toPrint, file=writeTo)
	print(toPrint)

}

main()
