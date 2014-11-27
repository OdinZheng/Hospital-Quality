rankhospital <- function(state, outcome, num = 'best'){
    
    data <- read.csv("./hospital/outcome-of-care-measures.csv", colClasses = "character", na.strings="Not Available")
    
    ## Check that state and outcome are valid
    valid_outcome <- c("heart attack", "heart failure", "pneumonia")
    valid_state <- unique(data[,7])
    # under stand the %in% operator and which opeartor
    # http://dr-k-lo.blogspot.com.au/2013/11/in-in-r-underused-in-operator.html
    
    if (!outcome %in% valid_outcome)
    {stop("invalid outcome")}
    
    if (!state %in% valid_state)
    {stop("invalid state")}
    
    if (! (num=="best" || num=="worst" || is.numeric(num))) 
    {stop("invalid num")}
    
    ## reduce the data to the state_data subset
    state_data <- data[data$State == state, ]
    
    # reduce the data to the corresponding outcome data (2 columns only)
    if (outcome == valid_outcome[1])
        {state_data <- state_data[,c(2,11)]}
    else if (outcome == valid_outcome[2])
        {state_data <- state_data[,c(2,17)]}
    else if (outcome == valid_outcome[3])
        {state_data <- state_data[,c(2,23)]}

    # remove NA
    state_data <- state_data[!is.na(state_data[,2]), ]

    sorted_index <- order(as.numeric(state_data[,2]), state_data[,1], decreasing = FALSE, na.last = NA)

    
    
    if (num == 'best'){
        num <- 1
        index <- sorted_index[num]
    }
    else if (num == 'worst'){
        index <- tail(sorted_index,1)        
    }
    else {index <- sorted_index[num]}
    
        
    return (state_data[index,1])
    
}