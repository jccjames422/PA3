rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  tdata <- read.csv('outcome-of-care-measures.csv', colClasses = 'character')
  ## change columns to appropriate class for analysis as numeric
  suppressWarnings(tdata[, 11] <- as.numeric(tdata[, 11]))
  suppressWarnings(tdata[, 17] <- as.numeric(tdata[, 17]))
  suppressWarnings(tdata[, 23] <- as.numeric(tdata[, 23]))
  
  ## Check that state, outcome, and num are valid
  states <- unique(tdata$State)
  if (!state %in% states) {
    stop('invalid state')
  } else if (!outcome %in% c('heart attack', 'heart failure', 'pneumonia')) {
    stop('invalid outcome')
  } else if (!num %in% c('best', 'worst')) {
    if (!is.integer(num)) {
      stop('invalid num')
    }
  }
  
  ## subset tdata frame matching only those rows with the selected state
  
  selected.state <- tdata[tdata$State == state, ]
  
  ## select hospital name and outcome columns
  
  if (outcome == 'heart attack') {
    selection <- 11
  } else if (outcome == 'heart failure') {
    selection <- 17
  } else if (outcome == 'pneumonia') {
    selection <- 23
  }
  
  chosen.outcome <- selected.state[, c(2, selection)]
  
  ## scrub na's out of the data
  
  out <- complete.cases(chosen.outcome)
  chosen.outcome <- chosen.outcome[out, ]
  
  ## sort the data frame based on outcome
  
  chosen.outcome <- chosen.outcome[with(chosen.outcome, order(chosen.outcome[[2]], chosen.outcome[[1]])), ]
  
  debug.result <- chosen.outcome
  
  debug.result
  
  ## create a data frame with Hospital Name, State, and 30-day death rate
  
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
}
