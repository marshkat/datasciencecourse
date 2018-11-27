complete <- function(directory, ids = 1:332){
        filename <- list.files(path = directory)
        id <- c(ids)
        nobs <- vector(length=0)
        for (val in ids){
                df <-read.csv(paste(directory,filename[val], sep= "/"))
                nobs <- append(nobs, sum(complete.cases(df)))
                
        }
        data.frame(id, nobs)
}