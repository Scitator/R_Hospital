# 2
best <- function(state, outcome) {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    ## Check that state and outcome are valid
    states <- data[ , 7]
    outcomes <- c("heart attack", "heart failure", "pneumonia")
    if (!(state %in% states)) {
        stop("invalid state")
    }
    if (!(outcome %in% outcomes)) {
        stop("invalid outcome")
    }
    ## Return hospital name in that state with lowest 30-day death
    stateData = subset(data, data$State == state)
    switch (outcome,
            "heart attack" = outcomeColumn <- 11,
            "heart failurek" = outcomeColumn <- 17,
            "pneumonia" = outcomeColumn <- 23
    )
    stateData <- stateData[complete.cases(stateData[,outcomeColumn]),]
    resIndex <- which.min(as.double(stateData[,outcomeColumn]))
    # 2 = Hospital.Name"
    stateData[resIndex,2]
}