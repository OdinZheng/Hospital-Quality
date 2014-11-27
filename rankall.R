rankall <- function(outcome, num="best")
{
    setwd("G:/Data Set for R/R programming")
    data <- read.csv("./hospital/outcome-of-care-measures.csv", colClasses = "character", na.strings="Not Available")
    
    ## Check that state and outcome are valid
    valid_outcome <- c("heart attack", "heart failure", "pneumonia")
    
    if (!outcome %in% valid_outcome)
    {stop("invalid outcome")} 
    
    if (! (num=="best" || num=="worst" || is.numeric(num))) 
    {stop("invalid num")}
    
    # reduce the data to the correpsonding outcome data (3 columns only)
    if (outcome == valid_outcome[1])
    {data <- data[,c(2,7,11)]}
    else if (outcome == valid_outcome[2])
    {data <- data[,c(2,7,17)]}
    else if (outcome == valid_outcome[3])
    {data <- data[,c(2,7,23)]}
    # data 1: hospital name, 2: state, 3: rate
    # remove NA in rate
    data <- data[!is.na(data[,3]), ]
    
    sorted_data <- data[order(as.numeric(data[,3]),data[,1], na.last = TRUE), ]   
   
    states <- sort(unique(sorted_data[,2]))

    
    # Function returns the hospital name for the given state at the specified rank.
    state_hospital_data <- function(state) {
        slice <- subset(sorted_data, State==state)
        num<-ifelse(num=="best",1, ifelse(num=="worst", nrow(slice), as.numeric(num)))
        #print(num)
        #fix(slice)
        slice <- slice[num, ]
        slice$State <- state
        return (slice)
    }
    state_data <- lapply(states, state_hospital_data)
    # http://www.stat.berkeley.edu/~s133/Docall.html
    output_frame <- as.data.frame(do.call(rbind, state_data), row.names=states)[,c(1,2)]

    colnames(output_frame) <- c("hospital", "state")
    return (output_frame)
    
    
}
    
    
    