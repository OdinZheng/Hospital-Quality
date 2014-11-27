best <- function (state, outcome){
    ## read outcome data
    setwd("G:/Data Set for R/R programming")
    data <- read.csv("hospital/outcome-of-care-measures.csv", colClasses = "character", na.strings="Not Available")
    
    ## Check that state and outcome are valid
    valid_outcome = c("heart attack", "heart failure", "pneumonia")
    valid_state = unique(data[,7])
    # under stand the %in% operator and which opeartor
    # http://dr-k-lo.blogspot.com.au/2013/11/in-in-r-underused-in-operator.html
    
    if (!outcome %in% valid_outcome)
        {stop("invalid outcome")}
    
    if (!state %in% valid_state)
        {stop("invalid state")}
    
    ## return hospital name in that state with lowest 30-day death rate
    ## reduce the data to the state_data subset
    state_data = data[data$State == state, ]
    ## remove the NA in 
    
    if (outcome == valid_outcome[1])
        {heart_attack_rate = as.numeric(state_data[,11])
        idx <- which.min(heart_attack_rate)}
    else if (outcome == valid_outcome[2])
        {heart_failure_rate = as.numeric(state_data[,17])
        idx <- which.min(heart_failure_rate)}
    else if (outcome == valid_outcome[3])
        {heart_failure_rate = as.numeric(state_data[,23])
        idx <- which.min(heart_failure_rate)}
    
    ## handling tie
    if (length(idx) > 1){
         ordered_idx = order(state_data[idx, 2])
         i = which.min(ordered_idx)
         idx = idx[i]}
        
    
    
    best_hospital = state_data[idx, 2]
    return (best_hospital)
    
}


