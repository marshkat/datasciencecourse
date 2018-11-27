best <- function(state, outcome) {
        options(warn = -1)
        ## Read outcome data (heart attack, heart failure, pneumonia)
        ds <- read.csv(file ="outcome-of-care-measures.csv", stringsAsFactors = FALSE)
        
        ## Check that state and outcome are valid
        if(state %in% unique(ds$State)){
        }
        
        else {
                message("invalid state")
                stop()
        }
        
        
        ##Create a data.frame for that state
        working <- subset(ds[,c(2,7,11,17,23)], State == state)
        
        
        ## Return hospital name in that state with lowest 30-day death 
        ## rate
                ## Heart Attack
        
        if(outcome == "heart attack"){
                index <- which.min(working[,3])
                return(working$Hospital.Name[index])
                options(warn = 0)
        }
                ## Heart Failure
        if(outcome == "heart failure"){
                index <- which.min(working[,4])
                return(working$Hospital.Name[index])
                options(warn = 0)
        }
                ##Pneumonia
        if(outcome == "pneumonia"){
                index <- which.min(working[,5])
                return(working$Hospital.Name[index])
                options(warn = 0)
        }
                
                ##Invalid outcome query     
        else {
                message("invalid outcome")
                options(warn = 0)
                stop()
        }
        
}
