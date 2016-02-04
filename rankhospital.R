rankhospital <- function(state, outcome, num = "best") {
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
    stateData = subset(data, data$State == state)
    switch (outcome,
            "heart attack" = outcomeColumn <- 11,
            "heart failure" = outcomeColumn <- 17,
            "pneumonia" = outcomeColumn <- 23
    )
    stateData <- stateData[complete.cases(stateData[,outcomeColumn]),]
    ## Return hospital name in that state with the given rank
    sortedStateData <- stateData[order(as.double(stateData[,outcomeColumn]), stateData[,2], decreasing=FALSE,na.last=NA),]
    #print (sortedStateData[,2])
    ## 30-day death rate
    if (num == "best") num <- 1
    if (num == "worst") num <- nrow(sortedStateData)
    sortedStateData[num, 2]
}