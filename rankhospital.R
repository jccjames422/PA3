rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  tdata <- read.csv('outcome-of-care-measures.csv', colClasses = 'character')
  ## change columns to appropriate class for analysis as numeric
  suppressWarnings(tdata[, 11] <- as.numeric(tdata[, 11]))
  suppressWarnings(tdata[, 17] <- as.numeric(tdata[, 17]))
  suppressWarnings(tdata[, 23] <- as.numeric(tdata[, 23]))
  
  ## Check that state and outcome are valid
  states <- unique(tdata$State)
  if (!state %in% states) {
    stop('invalid state')
  } else if (!outcome %in% c('heart attack', 'heart failure', 'pneumonia')) {
    stop('invalid outcome')
  }
  
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
}
