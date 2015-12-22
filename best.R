best <- function(state, outcome) {
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
  
  ## Return hospital name in that state with lowest 30-day death rate in
  ## selected state
  
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
  
  keeprows <- complete.cases(chosen.outcome)
  chosen.outcome <- chosen.outcome[keeprows, ]
  
  ## return the name(s) of the hospital(s) with the lowest value in the column
  hospitals <- chosen.outcome[chosen.outcome[[2]] == min(chosen.outcome[[2]]), 1]
  
  hospitals <- sort(hospitals)
  
  hospitals[[1]]
  
}
