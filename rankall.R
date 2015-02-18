rankall <- function (outcome, num =  'best') {
    
    if (!exists('hospital'))
        hospital <<- read.csv('outcome-of-care-measures.csv', colClasses = 'character')
    
    if ( !outcome %in% c('heart attack', 'heart failure', 'pneumonia') | is.na(outcome) ) { 
        stop('invalid outcome')
    }
    
    hospital <- hospital[, c(2,7,11,17,23)]
    hospital[, 3:5] <- sapply(hospital[, 3:5], as.numeric)
    
    out <- ifelse ( outcome == 'heart attack', 3, ifelse( outcome == 'heart failure', 4, 5))
    
    res <- data.frame()
    
    states <- unique(hospital[order(hospital$State),2])
    for (i in states) {   
        
        state <- hospital[which(hospital$State == i), ]
        
        if (num == 'worst') a <- nrow(state[which(!is.na(state[, out])), ])
        if (num == 'best') a <- 1
        if (is.numeric(num)) a <- num
        
        hs <- state[order(state[, out], state[, 1], na.last = T), 1]
        hs <- cbind(hospital = hs[a], state = i)
        res <- rbind(res, hs)
    }
    return(res)
    
}
