best <- function(state, outcome) {
    # best is a function that determines which hospital in a given state has
    # the best 30-day mortality for a specific outcome. It takes the name of
    # a state as the 2-character abbreviation and an outcome name.
    # 
    # Args:
    #     state: A 2-character abbreviation of the state
    #     outcome: A string with the outcome of care
    #
    # Returns:
    #     Name of the hospital with the best 30-day mortality for the outcome
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
    # Get minimum outcome value
    minOutcomeValue <- min(dataByState[,outcomeColumn], na.rm = TRUE)
    # Get row index where minimum outcome value occurs
    minOutcomeIndex <- which(dataByState[,outcomeColumn] == minOutcomeValue)
    
    # Sort resulting hospital names
    best <- sort(dataByState[minOutcomeIndex, "Hospital.Name"])
    # Return first name
    return(best[1]) 
}
