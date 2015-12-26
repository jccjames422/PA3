rankall <- function(outcome, num = "best") {
        #The function reads the outcome-of-care-measures.csv ﬁle and returns a 
        #2-column data frame containing the hospital in each state that has the 
        #ranking speciﬁed in num. For example the function call rankall("heart 
        #attack", "best") would return a data frame containing the names of the 
        #hospitals that are the best in their respective states for 30-day heart
        #attack death rates. The function should return a value for every state 
        #(some may be NA). The ﬁrst column in the data frame is named hospital, 
        #which contains the hospital name, and the second column is named state,
        #which contains the 2-character abbreviation for the state name. 
        #Hospitals that do not have data on a particular outcome are excluded 
        #from the set of hospitals when deciding the rankings. Handling ties. 
        #The rankall function should handle ties in the 30-day mortality rates 
        #in the same way that the rankhospital function handles ties.
        #
        #
        # Args: 
        #       outcome: must be one of three possible outcomes:  heart attack, 
        #       pneumonia, or heart failure.
        #
        #       num:  can be "best", "worst", or an integer indiciating the
        #       desired rank of the hospital
        #
        # Returns:
        #
        #       A 2-column data frame containing the names of the hospitals that
        #       are the best in their respective states for 30-day outcome
        #       rates. 
        
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
        
        ##scrub nas, for each state
        split.by.state <- lapply(split.by.state, function(x) {
                
                keeprows <- complete.cases(x)
                x <- x[keeprows, ]
        })
        
        ##sort by rank, for each state
        split.by.state <- lapply(split.by.state, function(x) {
                x <- x[ order(x[[2]], x[[1]]), ]
        })
        
        ## the hospital's rank is now the same as the row it is placed in.
        ## now I will add a new column to each data frame with the actual
        ## rank of that hospital
        
        debug.result <- lapply(split.by.state, function(x) {
                dr <- data.frame(x[[1]], x[[3]], x[[2]], c(1:nrow(x)),
                                 stringsAsFactors = FALSE)
                names(dr) <- c('Hospital.Name', 'State', 'Result', 'Rank')
                dr
        })
        
        ## For each state, find the hospital of the given rank
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
        
        final.result <- lapply(debug.result, function(x) {
                if (num == 'best') {
                        ## return the first row
                        result <- x[1, ]
                } else if (num == 'worst') {
                        ## return the last row
                        result <- x[max(x[[4]]), ]
                } else {
                        if (num > max(x[[4]])) {
                                result <- data.frame(NA, x[1,2], NA, NA)
                                names(result) <- c('Hospital.Name', 'State',
                                                   'Result', 'Rank')
                        } else {
                                ## return the selected row
                                result <- x[num, ]
                        }
                }
                result
        })
        
        
        
        ## now we need to change the data type of the final.result from a list
        ## of data frames, to each element of the list to be its own row in
        ## a single data frame.
        

        tmp.result <- data.frame()
        lapply(final.result, function(x) {
                tmp.result <<- rbind(tmp.result, x)     
        })
        
        ## now drop last two columns
        
        tmp.result <- data.frame(tmp.result[[1]], tmp.result[[2]],
                                 row.names = tmp.result[[2]])
        names(tmp.result) <- c('hospital', 'state')
        
        tmp.result
}