rankhospital <- function(state, outcome, num = "best") {
    # rankhospital is a function that determines the name of the hospital at a 
    # given state ranking for a given outcome. It takes the name of a state as
    # the 2-character abbreviation, an outcome name, and a rank value.
    # 
    # Args:
    #     state: A 2-character abbreviation of the state
    #     outcome: A string with the outcome of care
    #     num: "best", "worst", or integer indicating the ranking
    #
    # Returns:
    #     Name of hospital with the rank of num for the outcome in the state
    dataset <- read.csv("outcome-of-care-measures.csv", stringsAsFactors=FALSE,
                        na.strings="Not Available")
    
    # Check if state entered is present in data set
    if (!(state %in% dataset$State)) {
        stop("invalid state")
    }
    
    # Check if outcome is valid, and select correct column
    if (outcome == "heart attack") {
        outcomeColumn <- 11
    } else if (outcome == "heart failure") {
        outcomeColumn <- 17
    } else if (outcome == "pneumonia") {
        outcomeColumn <- 23
    } else {
        stop("invalid outcome")
    }
    
    # Data for the state
    dataByState <- subset(dataset, State == state)
    # Sort data by outcome then hospital name
    sortedData <- dataByState[order(dataByState[,outcomeColumn],
                                    dataByState$Hospital.Name),]
    
    # Get maximum outcome value
    outcomeValue <- max(sortedData[,outcomeColumn], na.rm = TRUE)
    # Get row index where maximum outcome value occurs
    maxOutcomeIndex <- which(sortedData[,outcomeColumn] == outcomeValue)
        
    if (num == "best") {
        outcomeIndex <- 1
    } else if (num == "worst") {
        outcomeIndex <- maxOutcomeIndex
    } else if (num >= 1 && num <= maxOutcomeIndex){
        outcomeIndex <- num
    } else {
        return(NA)
    }
        
    # Sort resulting hospital names
    best <- sort(sortedData[outcomeIndex, "Hospital.Name"])
    # Return first name
    return(best[1]) 
}