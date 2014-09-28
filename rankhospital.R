## rankhospital takes three arguments: the 2-character abbreviated name of a 
## state, an outcome name and the ranking of a hospital in that state for that outcome.
## The function reads the outcome-of-care-measures.csv file and returns a 
## character vector with the name of the hospital with the ranking specified 
## (best / worst / any integer indicating the ranking) for the specified
## outcome (heart attack / heart failure / pneumonia) in that state.


rankhospital <- function(state, outcome, num = "best") {
        ## Read csv file and store it in "care" dataframe, with column content  
        ## converted to character and "Not Available" to NA
        care <- read.csv("outcome-of-care-measures.csv", 
                         colClasses = "character", 
                         na.string = "Not Available")
 
        ## Create data frame of unique values for state abbreviated names (col. 7)
        uniquestate <- (unique(care[7]))
        
        ## Create vector of unique values for outcomes
        uniqueoutcome <- c("heart attack", "heart failure", "pneumonia")
        
        ## Check if state name is valid with vector of unique values
        if(state %in% uniquestate[[1]]) {
                
                ## Check if outcome name is valid in vector of unique values
                if(outcome %in% uniqueoutcome) {
                        
                        ## Rename columns of interest (3 columns with 30-days 
                        ## mortality rate) to match the name inserted in "outcome"
                        colnames(care)[11] <- "heart attack"
                        colnames(care)[17] <- "heart failure"
                        colnames(care)[23] <- "pneumonia"
                        
                        ## Subset dataframe with state of interest
                        ## using state abbreviated name inserted in "state"
                        hosp <- subset(care, care[7] == state)
                        
                        ## Order output by increasing values for outcome (to have 
                        ## lowest value in first row(s)) then by increasing / 
                        ## alphabetical values for hospital name 
                        ## (to handle ties between hospitals)
                        ordered <- hosp[order(as.numeric(hosp[[outcome]]), 
                                              hosp$Hospital.Name), ]        
                        
                        ## Subset dataframe with rows where value is not missing
                        ## in column outcome (i.e. exclude rows where outcome = NA)
                        goodordered <- subset(ordered, !is.na(ordered[[outcome]]))
                        
                        ## Create a subset of dataframe with hospital names
                        ## (ordered and without NA)
                        hospname <- goodordered$Hospital.Name
                                
                        ## Subset hospital name on first value if num = "best"
                        if(num == "best") {
                                result <- hospname[1]
                        
                        ## Subset hospital name on last value if num = "worst"
                        } else if (num == "worst") {
                                result <- tail(hospname, 1)
                        
                        ## Otherwise, subset hospital name on row inserted in "num"
                        } else {
                                result <- hospname[num]   
                        }
                                                        
                        return(result)
                
                ## If outcome is not valid, returns error msg
                } else {
                        stop("invalid outcome")}
        
        ## If state name is not valid, returns error msg
        } else {
                stop("invalid state")
        }
}       