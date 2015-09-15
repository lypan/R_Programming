rankall <- function(outcome, num = "best") {
  col_map <- c(11, 17, 23)
  names(col_map) <- c("heart attack","heart failure","pneumonia")
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ## Check that state and outcome are valid
  if (!outcome %in% names(col_map)) {
    stop("invalid outcome")
  }
  hospital <- vector("character", 0)
  state <- vector("character", 0)
  ## For each state, find the hospital of the given rank
  total_state <- sort(unique(data[, "State"]))
  for (s in total_state) {
    temp <- data[data$State == s, ]
    result <- suppressWarnings(temp[order(as.numeric(temp[, col_map[outcome]]), temp[, 2], na.last = NA), ])
    state <- c(state, result[1, "State"])
    if (num == "best") {
      hospital <- c(hospital, result[1,2])    
    }
    else if (num == "worst") {
      hospital <- c(hospital, result[nrow(result), 2]) 
    }
    else if(num > nrow(result)) {
      hospital <- c(hospital, NA) 
    }
    else {
      hospital <- c(hospital, result[num, 2]) 
    }
  }
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  data.frame(hospital=hospital,state=state, row.names=sort(unique(data[, "State"])))
}