complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  base_dir <- paste(getwd(), directory, sep="/")
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  first <- vector("numeric", 0)
  second <- vector("numeric", 0)
  for (i in id) {
    csv_name <- sprintf("%03d.csv", i)
    csv_path <- paste(base_dir, csv_name, sep="/")
    csv <- read.csv(csv_path)
    # calculate how many true
    n <- sum(complete.cases(csv))
    first <- c(first, i)
    second <- c(second, n)
  }
  result <- data.frame(id=first, nobs=second)
  # print(result)
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  return(result)
}