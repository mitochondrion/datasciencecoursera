# source("complete.R")

corr <- function(directory, threshold = 0) {
#   nobs_data = complete(directory)
#   ids = nobs_data[nobs_data$nobs > threshold,][["id"]]
    
#    len = length(ids)
   correlations = rep(NA, 332)
#   
  for (i in 1:332) {
    path = paste(directory,
                 "/",
                 formatC(i, width=3, format="d", flag="0"),
                 ".csv",
                 sep="")
    data = read.csv(path, header=TRUE)
    complete_data = data[ !is.na(data$sulfate) & !is.na(data$nitrate), ]
    
    if (nrow(complete_data) < threshold) {
      next
    }
    
    correlations[i] = cor(complete_data$nitrate, complete_data$sulfate)
  }

  correlations[!is.na(correlations)]
}