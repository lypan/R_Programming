spollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  base_dir <- paste(getwd(), directory, sep="/")
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".

  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  total_mean <- 0
  data <- 0
  for (i in id) {
    csv_namee <- sprintf("%03d.csv", i)
    csv_path <- paste(base_dir, csv_namee, sep="/")
    csv <- read.csv(csv_path)
    # concatenate data
    data <- c(csv[[pollutant]], data)
    ##total_mean = total_mean + mean(csv[[pollutant]], na.rm = T)
  }
  ##print(data)
  total_mean <- mean(data[complete.cases(data)])
  # total_mean <- mean(data, na.rm = T)
  # print(total_mean)
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  ## NOTE: Do not round the result!
  return(total_mean)
}