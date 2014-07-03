## Given a state abbreviation and outcome of interest, the "best" function will return the name of the hospital in the specified state with the lowest 30-day mortality rate for the given outcome.

best <- function(state,outcome){
    ## Read the outcome data
    df <- read.csv("outcome-of-care-measures.csv", na.strings="Not Available")
    validOutcomes <- c("heart attack","heart failure","pneumonia")
    
    ## Check that state and outcome variables are valid
    if ((state %in% df$State)==FALSE) stop("invalid state")
    if ((outcome %in% validOutcomes)==FALSE) stop("invalid outcome")
    
    ## Return hospital name in specified state with lowest 30-day mortality rate
    x <- subset(df, df$State == state)
    names(x)[c(11,17,23)] <- c("HA","HF","P")
    x <- subset(x, select = c(Hospital.Name, HA, HF, P))
    
    if (outcome == "heart attack"){
        minHA <- min(x$HA, na.rm=TRUE)
        x1 <- subset(x, x$HA == minHA)
        x1 <- x1[order(1),]
        x1$Hospital.Name[1]}
    else if (outcome == "heart failure"){
        minHF <- min(x$HF, na.rm=TRUE)
        x1 <- subset(x, x$HF == minHF)
        x1 <- x1[order(1),]
        x1$Hospital.Name[1]}
    else {
        minP <- min(x$P, na.rm=TRUE)
        x1 <- subset(x, x$P == minP)
        x1 <- x1[order(1),]
        x1$Hospital.Name[1]}
}
