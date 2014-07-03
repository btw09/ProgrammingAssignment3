rankhospital <- function(state,outcome,num){
    ## Read the outcome data
    df <- read.csv("outcome-of-care-measures.csv", na.strings="Not Available")
    validOutcomes <- c("heart attack","heart failure","pneumonia")
    
    ## Check that state and outcome variables are valid
    if ((state %in% df$State)==FALSE) stop("invalid state")
    if ((outcome %in% validOutcomes)==FALSE) stop("invalid outcome")
    
    ## Return hospital name in specified state with requested ranking
    x <- subset(df, df$State == state)
    names(x)[c(11,17,23)] <- c("HA","HF","P")
    
    if (outcome == "heart attack"){
        x <- subset(x, !is.na(HA), select = c(Hospital.Name,HA))
        x <- x[order(x[2],x[1]),]
        if (num == "best"){
          x$Hospital.Name[1]}
        else if (num == "worst"){
          x$Hospital.Name[nrow(x)]}
        else {
          x$Hospital.Name[num]}
    }
    else if (outcome == "heart failure"){
        x <- subset(x, !is.na(HF), select = c(Hospital.Name,HF))
        x <- x[order(x[2],x[1]),]
        if (num == "best"){
          x$Hospital.Name[1]}
        else if (num == "worst"){
          x$Hospital.Name[nrow(x)]}
        else {
          x$Hospital.Name[num]}
    }
    else {
        x <- subset(x, !is.na(P), select = c(Hospital.Name,P))
        x <- x[order(x[2],x[1]),]
        if (num == "best"){
          x$Hospital.Name[1]}
        else if (num == "worst"){
          x$Hospital.Name[nrow(x)]}
        else {
          x$Hospital.Name[num]}
    }
}
