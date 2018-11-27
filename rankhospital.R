rankhospital <- function(state, outcome, num = "best") {
        ## Read outcome data
        ds <- read.csv(file ="outcome-of-care-measures.csv", stringsAsFactors = FALSE)
        ds <- transform(ds, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack = as.numeric(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack), 
                        Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure = as.numeric(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),
                        Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia = as.numeric(Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
        ## Check that state and outcome are valid
        if(state %in% unique(ds$State)){
        }
        
        else {
                message("invalid state")
                stop()
        }
        ## Return hospital name in that state with the given rank
        dec <- FALSE
        if(num == "best"){
                num <-1
                dec <- FALSE
        }
        if(num == "worst"){
                num <- 1
                dec<- TRUE
        }
        
        
       # working <- subset(transform(ds[,c(2,7,11,17,23)]), State == state)
        if(outcome == "heart attack"){
                working <- subset(ds[,c(2,7,11)], State == state)
                new_working <- working[order(working$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, 
                                             working$Hospital.Name, decreasing = dec),]
                return(new_working$Hospital.Name[num])
        }
        ## Heart Failure
        if(outcome == "heart failure"){
                working <- subset(ds[,c(2,7,17)], State == state)
                new_working <- working[order(working$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,
                                             working$Hospital.Name, decreasing = dec),]
                return(new_working$Hospital.Name[num])
        }
        ##Pneumonia
        if(outcome == "pneumonia"){
                working <- subset(ds[,c(2,7,23)], State == state)
                new_working <- working[order(working$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,
                                             working$Hospital.Name, decreasing = dec),]
                return(new_working$Hospital.Name[num])
        }
        
        ##Invalid outcome query     
        else {
                message("invalid outcome")
                stop()
        }
        
}
