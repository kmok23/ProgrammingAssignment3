rankall <- function(outcome, num = "best") {
    # rankall is a function that determines the names of the hospitals at a 
    # given ranking for a given outcome. It takes an outcome name and a rank
    # value.
    # 
    # Args:
    #     outcome: A string with the outcome of care
    #     num: "best", "worst", or integer indicating the ranking
    #
    # Returns:
    #     Data frame with the names of hospitals with the rank of num for every
    #     state
    rawData <- read.csv("outcome-of-care-measures.csv", stringsAsFactors=FALSE,
                        na.strings="Not Available")
    
    dataset <- rawData[,c(2,7,11,17,23)]
    
    # Check if outcome is valid, and select correct column
    if (outcome == "heart attack") {
        outcomeColumn <- 3
    } else if (outcome == "heart failure") {
        outcomeColumn <- 4
    } else if (outcome == "pneumonia") {
        outcomeColumn <- 5
    } else {
        stop("invalid outcome")
    }
    
    # Data split by states
    dataByState <- split(dataset, dataset$State)
    # Sort data by outcome then hospital name
    sortedData <- lapply(dataByState, function(x) {
        x[order(x[,outcomeColumn], x$Hospital.Name),]
    } )
    
    #result <- data.frame(hospital = character(), state = character(),
    #                     stringsAsFactors=FALSE)
    for (i in names(sortedData)) {
        # Get maximum outcome value
        outcomeValue <- max(sortedData[[i]][,outcomeColumn], na.rm = TRUE)
        # Get row index where maximum outcome value occurs
        maxOutcomeIndex <- which(sortedData[[i]][,outcomeColumn] == outcomeValue)
        if (num == "best") {
            outcomeIndex <- 1
        } else if (num == "worst") {
            outcomeIndex <- maxOutcomeIndex
        } else if (num >= 1 && num <= maxOutcomeIndex){
            outcomeIndex <- num
        } else {
            result[i,] <- c("<NA>", i)
            next
        }
        result[i,] <- c(sortedData[[i]][outcomeIndex,"Hospital.Name"],
                        sortedData[[i]][outcomeIndex,"State"])
    }
    
    # Return data frame
    return(result)
}