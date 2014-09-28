## rankall takes two arguments: an outcome name and the ranking of a hospital 
## for that outcome.
## The function reads the outcome-of-care-measures.csv file and returns a 
## 2-column data frame containing the hospital in each state that has the ranking 
## specified (best / worst / any integer indicating the ranking) for the specified
## outcome (heart attack / heart failure / pneumonia).

rankall <- function(outcome, num = "best") {
        ## Read csv file and store it in "care" dataframe, with column content  
        ## converted to character and "Not Available" to NA
        care <- read.csv("outcome-of-care-measures.csv", 
                         colClasses = "character", 
                         na.string = "Not Available")
 
        ## Rename columns of interest (3 columns with 30-days 
        ## mortality rate) to match the name inserted in "outcome"
        colnames(care)[11] <- "heart attack"
        colnames(care)[17] <- "heart failure"
        colnames(care)[23] <- "pneumonia"
        
        ## Order dataframe by increasing / alphabetical values for state name
        care <- care[order(care$State), ]
        
        ## Create vector of unique values for outcomes
        uniqueoutcome <- c("heart attack", "heart failure", "pneumonia")
        
        ## Create vector of unique values for state abbreviated names (col. 7)
        uniquestate <- (unique(care[7]))
        uniquestatevector <- uniquestate[[1]]
        
        ## Create data frame that will contain final output
        final <- data.frame()
        
        ## For loop repeating action for each state
        for(i in seq_along(uniquestatevector)) {
                        
                ## Check if outcome name is valid in vector of unique values
                if(outcome %in% uniqueoutcome) {
                        
                        ## Subset dataframe for each state of interest
                        hosp <- subset(care, care[7] == uniquestatevector[i])
                                                        
                        ## Order output by increasing values for outcome (to have 
                        ## lowest value in first row(s)) then by increasing / 
                        ## alphabetical values for hospital name 
                        ## (to handle ties between hospitals)
                        ordered <- hosp[order(as.numeric(hosp[[outcome]]), 
                                              hosp$Hospital.Name), ]        
                
                        ## Subset dataframe with rows where value is not missing
                        ## in column outcome (i.e. exclude rows where outcome = NA)
                        goodordered <- subset(ordered, !is.na(ordered[[outcome]]))
                        
                                ## Create a subset of dataframe on hospital names
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
                                
                        ## Bind a second column containing state name with result
                        result <- cbind(result, uniquestatevector[i])
                        
                        ## Bind resulting row with existing "final" data frame
                        final <- rbind(final, result)
                        
                                                
                ## If outcome is not valid, returns error msg
                } else {
                        stop("invalid outcome")}
        }
        
        ## Change row names (to state names) and column names
        rownames(final) <- final[, 2]
        colnames(final) <- c("hospital", "state")
        
        return(final)
}       