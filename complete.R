complete <- function(directory, id = 1:332){
  full_files <- list.files(directory, full.names = T)
  res <- data.frame()
  for (i in id) {
    file <- read.csv(full_files[i])
    nobs <- nrow(file[which(!is.na(file$sulfate) & !is.na(file$nitrate)),])
    nobs <- c(i, nobs)
    res <- rbind(res, nobs)
  }
  names(res) <- c('id', 'nobs')
  return(res)
}
