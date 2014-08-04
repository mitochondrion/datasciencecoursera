complete = function(directory, id = 1:332) {
  len = length(id)
  nobs = numeric(len)
  
  for (i in 1:len) {
    path = paste(directory,
                 "/",
                 formatC(id[i], width=3, format="d", flag="0"),
                 ".csv",
                 sep="")
    data = read.csv(path, header=TRUE)
    complete_data = data[ !is.na(data$sulfate) & !is.na(data$nitrate), ]
        
    nobs[i] = nrow(complete_data)
  }
  
  data.frame(id, nobs)
}