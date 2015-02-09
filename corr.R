
corr <- function(directory, threshold = 0) {
  full_files <- list.files(directory, full.names = T)
  res <- integer()
  for (i in full_files) {
    file <- read.csv(i)
    if (nrow(file[which(!is.na(file$nitrate) & !is.na(file$sulfate)),]) > threshold){
      res<- rbind(res, cor(file$nitrate, file$sulfate, use = 'complete.obs'))
      res <- res
    }
  }
  return(res)
}
