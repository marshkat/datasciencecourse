rankall <- function(outcome, num = "best") {
        ## Read outcome data
        ds <- read.csv(file ="outcome-of-care-measures.csv", stringsAsFactors = FALSE)
        ds <- transform(ds, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack = as.numeric(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack), 
                        Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure = as.numeric(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),
                        Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia = as.numeric(Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
        
        ## Check that outcome is valid
        if(outcome == "heart attack"){
                settype <- 11
        }
        
        if(outcome == "heart failure"){
                settype <- 17
        }
        if(outcome == "pneumonia"){
                settype <- 23
        }
        #else {
        #        message("invalid outcome")
        #        stop()
        #}
        
        dec <- FALSE
        if(num == "best"){
                num <-1
                dec <- FALSE
        }
        if(num == "worst"){
                num <- 1
                dec<- TRUE
        }
        
        states <- sort(unique(ds$State))
        hospitals<- vector(length = 0)
        
        ## For each state, find the hospital of the given rank
        for(state in states){
                working <- subset(ds[,c(2,7,settype)], State == state)
                new_working <- working[order(working[,3], working$Hospital.Name, decreasing = dec),]
                hospitals <- append(hospitals, new_working[num,1])
        }
        
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
        hospitalsbystate <- data.frame(states, hospitals)
        hospitalsbystate
}

