basketAnalysis <- function(inputData, basketColumn, splittingCriteria = NULL, support = 0.1, confidence = 0.8, minLength = 2) {
  if (!require("arules")) {
    install.packages("arules", dep = TRUE)
  }
  require(arules)
  if (is.data.frame(inputData) == FALSE) {
    stop("'inputData' must be a data frame.")
  }
  if (is.character(basketColumn) == FALSE) {
    stop("'basketColumn' must be character.")
  }
  if (is.character(splittingCriteria) == FALSE & is.null(splittingCriteria) == FALSE) {
    stop("'splittingCriteria' must be character, if specified.")
  }
  if (is.numeric(support) == FALSE | is.numeric(confidence) == FALSE) {
    stop("'support' and 'confidence' must be numeric.")
  }
  if ((support > 0 & support <= 1) == FALSE | (confidence > 0 & confidence <= 1) == FALSE) {
    stop("'support' and 'confidence' must be greater than 0 and less than or equal to 1.")
  }
  if (minLength %% 1 != 0) {
    stop("minLength must be an integer.")
  }
  basketOutput <- data.frame()
  if (is.null(splittingCriteria) == TRUE) {
    write(inputData[, basketColumn], "rules")
    transactions <- read.transactions("rules", format = "basket", sep = ",", rm.duplicates = TRUE)
    basketRules <- apriori(transactions, parameter = list(target = "rules", sup = support, minlen = minLength, confidence = confidence))
    basketOutput <- as(basketRules, "data.frame")
  }
  if (is.null(splittingCriteria) == FALSE) {
    basketOutput <- data.frame()
    splittingCriteria <- unique(inputData[,splittingCriteria])
    for (i in splittingCriteria) {
      write(inputData[inputData[,splittingCriteria] == i, basketColumn], "rules")
      transactions <- read.transactions("rules", format = "basket", sep = ",", rm.duplicates = TRUE)
      basketRules <- apriori(transactions, parameter = list(target = "rules", sup = support, minlen = minLength, confidence = confidence))
      aprioriRules <- as(basketRules, "data.frame")
      aprioriRules$splittingCriteria <- i
      basketOutput <- rbind(basketOutput, aprioriRules)
    }
  }
  assign("basketOutput", basketOutput, envir = .GlobalEnv)
}