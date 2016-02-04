rankall <- function(outcome, num = "best") {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    ## Check that state and outcome are valid
    outcomes <- c("heart attack", "heart failure", "pneumonia")
    if (!(outcome %in% outcomes)) {
        stop("invalid outcome")
    }
    switch (outcome,
            "heart attack" = outcomeColumn <- 11,
            "heart failure" = outcomeColumn <- 17,
            "pneumonia" = outcomeColumn <- 23
    ) 
    hospital<-character(0)
    validState = sort(unique(data[,7]))
    for (i in seq_along(validState)) {
        dataState <- data[data$State==validState[i],]
        sortedDataState <- dataState[order(as.numeric(dataState[,outcomeColumn]), dataState[,2], decreasing=FALSE, na.last=NA), ]
        this.num = num
        if (this.num=="best") this.num = 1
        if (this.num=='worst') this.num = nrow(sortedDataState)
        
        hospital[i] <- sortedDataState[this.num, 2]
    }
    data.frame(hospital=hospital,state=validState,row.names=validState)
}