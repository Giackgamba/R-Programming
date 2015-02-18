rankhospital <- function (state, outcome, num = 'best'){
    
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
    
    if (num == 'best') num <- 1
    if (num == 'worst') num <- nrow(hospital[which(!is.na(hospital[, out])), ])
    if (num > nrow(hospital)) return(NA)
    if (is.numeric(num)) {
        res <- hospital[order(hospital[, out], hospital[, 1], na.last = T), 1]
    }
    
    return(res[num])
    
}


