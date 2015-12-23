rankall <- function(outcome, num = "best") {
        
        ## Read outcome data
        
        tdata <- read.csv('outcome-of-care-measures.csv',
                          colClasses = 'character')
        
        ## change columns to appropriate class for analysis as numeric
        suppressWarnings(tdata[, 11] <- as.numeric(tdata[, 11]))
        suppressWarnings(tdata[, 17] <- as.numeric(tdata[, 17]))
        suppressWarnings(tdata[, 23] <- as.numeric(tdata[, 23]))
        
        ## Check that outcome and num are valid
        
        if (!outcome %in% c('heart attack', 'heart failure', 'pneumonia')) {
                stop('invalid outcome')
        } else if (!num %in% c('best', 'worst')) {
                if (!is.integer(as.integer(num))) {
                        stop('invalid num')
                }
        }
        
        ## select hospital name, outcome, and state columns
        
        if (outcome == 'heart attack') {
                selection <- 11
        } else if (outcome == 'heart failure') {
                selection <- 17
        } else if (outcome == 'pneumonia') {
                selection <- 23
        }
        
        chosen.outcome <- tdata[, c(2, selection, 7)]
        
        names(chosen.outcome) <- c('hospital', outcome, 'state')
        
        ##this returns a list of data frames broken up by state
        split.by.state <- split(chosen.outcome, chosen.outcome$state)
        
        ##scrub nas
        split.by.state <- lapply(split.by.state, function(x) {
                
                keeprows <- complete.cases(x)
                x <- x[keeprows, ]
        })
        
        ##sort by rank
        split.by.state <- lapply(split.by.state, function(x) {
                x <- x[ order(x[[2]], x[[1]]), ]
        })
        
        split.by.state
        
        ## rank all hospitals by state
        ## will produce a data frame with hospital, state, and rank as columns
        
        ## For each state, find the hospital of the given rank
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
}
