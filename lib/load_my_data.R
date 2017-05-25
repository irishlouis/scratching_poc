
#' Title
#'
#' @param datafolder 
#' @param epoch_length 
#'
#' @return
#' @export
#'
#' @examples
load.data <- function(datafolder = NULL, epoch_length){
  ifelse (!is.null(datafolder),
          csv.files <- paste0("data/", datafolder,"/", list.files(paste0("data/", datafolder))),
          csv.files <- paste0("data/", list.files(paste0("data"))))
  
  # get the csv files
  csv.files <- csv.files[grep(".csv", csv.files)]
  
  # empty list to hold raw data
  data <- list()
  # allow for milliseconds
  options(digits.secs=3)
  
  # for each RAW csv file
  for (i in csv.files[str_detect(csv.files, "RAW.csv")]){
    dt <- fread(i, header = T, stringsAsFactors = F)
    
    device_id <- strsplit(strsplit(i, " ")[[1]][1], "/")[[1]][ifelse(is.null(datafolder),
                                                                     0,
                                                                     length(strsplit(datafolder, "/")))+2]
    dt[,':='(
      device_id = device_id,
      datetime = dmy_hms(Timestamp),
      Timestamp = NULL,
      vec.mag = sqrt(`Accelerometer X`^2 + `Accelerometer Y`^2 + `Accelerometer Z`^2),
      `Accelerometer X` = NULL,
      `Accelerometer Y` = NULL,
      `Accelerometer Z` = NULL
    ),][,':='(
      time_minute = floor_date(datetime, "minute"),
      second = epoch_length * floor(second(datetime)/epoch_length)
    ),][,':='(
      epoch_id = time_minute + seconds(second),
      time_minute = NULL,
      second = NULL
    ),]
    
    data[[which(csv.files[str_detect(csv.files, "RAW.csv")] == i)]] <- dt
  }
  rm(dt)
  # collapse to single file
  data <- rbindlist(data)
  setkey(data, device_id, epoch_id)
  return(data)
}