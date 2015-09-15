best <- function(state, outcome) {
  col_map <- c(11, 17, 23)
  names(col_map) <- c("heart attack","heart failure","pneumonia")
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ## Check that state and outcome are valid
  if (!state %in% unique(data[, "State"])) {
    stop("invalid state")
  }
  else if (!outcome %in% names(col_map)) {
    stop("invalid outcome")
  }
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  data <- data[data$State == state, ]
  result <- suppressWarnings(data[order(as.numeric(data[, col_map[outcome]]), data[, 2], na.last = NA), ])
  result[1, 2]
}