pollutantmean = function(directory, pollutant, id=1:332) {
  all_measurements <- c()
  
  for (i in id) {
    path = paste(directory,
                  "/",
                  formatC(i, width=3, format="d", flag="0"),
                  ".csv",
                  sep="")
    data = read.csv(path, header=TRUE)
    readings = data[[pollutant]]
    readings = readings[!is.na(readings)]
    all_measurements = c(all_measurements, readings)
#     print(data[1:5,])
#     print(paste("path:", path))
#     print(paste("all_measurements LENGTH:", length(all_measurements)))
  }
  
  mean(all_measurements)
}
