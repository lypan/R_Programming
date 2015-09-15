corr <- function(directory, threshold=0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  base_dir <- paste(getwd(), directory, sep="/")
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  cor_vec <- vector("numeric", 0)
  id <- 1:332
  for (i in id) {
    csv_name <- sprintf("%03d.csv", i)
    csv_path <- paste(base_dir, csv_name, sep="/")
    csv <- read.csv(csv_path)
    n <- sum(complete.cases(csv))
    if (n > threshold) {
      ## print(i)
      c_v <- cor(csv[["sulfate"]], csv[["nitrate"]], use="complete.obs")
      cor_vec <- c(cor_vec, c_v)
    }

  }
  # print(cor_vec)
  ## Return a numeric vector of correlations
  ## NOTE: Do not round the result!
  return(cor_vec)
}