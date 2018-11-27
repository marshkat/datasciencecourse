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



corr <- function(directory, threshold = 0){
        cc <- complete(directory) #create dataframe which counts the number of complete cases in each file
        filename <- list.files(path = directory) #create a lookup vector for filenames
        corrvec <- numeric(length = 0) #create empty vector which will later hold correlation values
        for(i in 1:length(cc[,2])){
                if (cc[,2][i] > threshold){
                        df <- read.csv(paste(directory,filename[i], sep = "/"))
                        corrvec <- append(corrvec, cor(df$nitrate,df$sulfate, use = "complete.obs"))
                }
        }
        corrvec
}