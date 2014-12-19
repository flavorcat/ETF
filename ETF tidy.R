long.data <- function(data){
    #accepts data in original messy format and combines various date columns to a single variable.
    long.data <- as.data.frame(matrix(nrow=0,ncol=3))
    names <- colnames(data)
    for (n in 1:8){
        cols <- c(2*n-1, 2*n)
        temp.data <- data[,cols]
        variable.name <- names[2*n]
        names.vector <- rep(variable.name, nrow(temp.data))
        temp.data <- cbind(temp.data, names.vector)
        temp.data <- temp.data[c(1,3,2)]
        colnames(temp.data) <- c("date", "variable", "value") 
        long.data <- rbind(long.data, temp.data)
    }
    long.data$date <- as.POSIXct(long.data$date, format = '%e/%m/%Y')
    return(long.data)
}