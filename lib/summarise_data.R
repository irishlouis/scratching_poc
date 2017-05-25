
#' Title
#'
#' @param data 
#' @param human.devices 
#'
#' @return
#' @export
#'
#' @examples
summarise.data <- function(data, human.devices){
  # create summary data set 
  p <- proc.time()
  # get summary data for vector magnitude
  ## exclude epochs with low step data
  setkey(data, device_id, epoch_id, steps_bin)
  data <- data[complete.cases(data)]
  vector.summary <- data[steps_bin != "0-2", 
                         .(avg.vec = mean(vec.mag),
                           sd.vec = sd(vec.mag),
                           steps = steps,
                           steps_bin = steps_bin), 
                         .(device_id, epoch_id) ] %>% setkey(device_id, epoch_id)
  
  # get summary data for peak summary
  peak.summary <- data[steps_bin != "0-2", 
                       get.peak.summary(vec.mag, k = 25, freq = 100), 
                       .(device_id, epoch_id) ] %>% setkey(device_id, epoch_id)
  (proc.time() - p)[3]
  
  # join summary data
  summary <- vector.summary[peak.summary]
  
  # add label for human / dog
  summary[,subj_type:=ifelse(summary$device_id %in% human.devices, 
                             "human", "dog"),]
  return(unique(summary))
}