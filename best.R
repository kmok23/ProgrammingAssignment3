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
    dataset <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    if (outcome == "heart attack") {
        outcomeColumn <- 11
    } elseif (outcome == "heart failure") {
        outcomeColumn <- 17
    } elseif (outcome == "pneumonia") {
        outcomeColumn <- 23
    } else {
        stop("invalid state")
    }
    
    split(dataset[outcomeColumn], dataset[[state]])
    
    columnNumber <- which(names(dataset) == outcome) # Column of the outcome
    dataByState <- dataset[which(dataset$State == state),] # Data for the state
    
    minOutcomeValue <- min(dataByState[[outcome]])
    
    best <- dataByState[which(dataByState[[outcome]] == minOutcomeValue), "Hospital.Name"]
    return(best)
}
