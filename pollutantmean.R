pollutantmean <- function(directory, pollutant, id = 1:332){
  
  files_full <- list.files(directory, full.names = T)
  dat <- data.frame()
  for (i in id) {
    dat <- rbind(dat, read.csv(files_full[i]))    
  }
  res <- round(mean(dat[, pollutant], na.rm = T), digits = 3)
  return(res)
}

