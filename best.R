best <- function(state, outcome) {
    
    hospital <- read.csv('outcome-of-care-measures.csv', colClasses = 'character')

    if ( !state %in% unique(hospital$State) | is.na(state) ) {
        stop('invalid state')
    }
    if ( !outcome %in% c('heart attack', 'heart failure', 'pneumonia') | is.na(outcome) ) { 
        stop('invalid outcome')
    }
    
    hospital <- hospital[which(hospital$State == state), c(2,7,11,17,23)]
    hospital[, 3:5] <- sapply(hospital[, 3:5], as.numeric)
    
    out <- ifelse ( outcome == 'heart attack', 3, ifelse( outcome == 'heart failure', 4, 5))
    res <- head(hospital[order(hospital[, out], na.last = T), 1], 1)
    return(res)
}
