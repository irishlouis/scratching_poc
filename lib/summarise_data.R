
#' Title
#'
#' @param data 
#'
#' @return
#' @export
#'
#' @examples
summarise.data <- function(data){
  # create summary data set 
  p <- proc.time()
  # get summary data for vector magnitude
  ## exclude epochs with low step data
  setkey(data, device_id, epoch_id)
  data <- data[complete.cases(data)]
  vector.summary <- data[, 
                         .(avg.vec = mean(vec.mag),
                           sd.vec = sd(vec.mag)), 
                         .(device_id, epoch_id) ] %>% 
    setkey(device_id, epoch_id)
  
  # get summary data for peak summary
  ## break up into 10e6 chunks if large
  if (nrow(data) < 10e6){
    peak.summary <- data[, 
                         get.peak.summary(vec.mag, k = 25, freq = 100), 
                         .(device_id, epoch_id) ] %>% 
      setkey(device_id, epoch_id)
  } else {
    tmp1 <- lapply(1:floor(nrow(data) / 1e6), 
                   function(x) {
                     message(paste(x, "-", Sys.time()))
                     return(data[(((x-1) * 1e6) + 1):(x * 1e6), 
                                    get.peak.summary(vec.mag, k = 25, freq = 100), 
                                   .(device_id, epoch_id) ] %>% 
                                setkey(device_id, epoch_id))})
    tmp1[[floor(nrow(data) / 1e6) + 1]] <- data[((floor(nrow(data) / 1e6) * 1e6) + 1):(nrow(data)), 
                 get.peak.summary(vec.mag, k = 25, freq = 100), 
                 .(device_id, epoch_id) ] %>% 
      setkey(device_id, epoch_id)
    
    peak.summary <- rbindlist(tmp1)
    rm(tmp1)
  }

  (proc.time() - p)[3]
  
  # join summary data
  summary <- vector.summary[peak.summary]
  
  return(unique(summary))
}