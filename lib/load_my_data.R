
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
  if (!is.null(datafolder)) {
    files <- paste0("data/", datafolder,"/", list.files(paste0("data/", datafolder)))  
  } else {
    files <- paste0("data/", list.files(paste0("data")))  
  }
         
  # get the csv files
  csv.files <- files[grep(".csv", files)]
  
  # empty list to hold raw data
  data <- list()
  # allow for milliseconds
  options(digits.secs=3)
  
  # for each RAW csv file
  for (i in csv.files[str_detect(csv.files, "RAW.csv")]){
    dt <- fread(i, header = T, stringsAsFactors = F)
    
    # device_id <- strsplit(strsplit(i, "_")[[1]][1], "/")[[1]][ifelse(is.null(datafolder),
    #                                                                  0,
    #                                                                  length(strsplit(datafolder, "/")))+2]
    # 
    # if(length(strsplit(device_id, " ")[[1]]) > 1) {
    #   device_id <- strsplit(device_id, " ")[[1]][1]
    # }
    device_id <- "TAS1E35150309"
   
    if (nrow(dt) < 10e6){
      dt[,':='(
        device_id = device_id,
        epoch_id = paste0(substr(Timestamp, 1, 18), 0),
        datetime = Timestamp,
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
    } else {
      tmp1 <- lapply(1:floor(nrow(dt) / 1e5), 
                     function(x) {
                       message(paste(x, "-", Sys.time()))
                       return(dt[(((x-1) * 1e5) + 1):(x * 1e5)] %>% 
                                mutate(device_id = device_id,
                                       epoch_id = paste0(substr(Timestamp, 1, 18), 0),
                                       datetime = Timestamp,
                                       vec.mag = sqrt(`Accelerometer X`^2 + `Accelerometer Y`^2 + `Accelerometer Z`^2)) %>%
                                mutate(time_minute = floor_date(datetime, "minute"),
                                       second = epoch_length * floor(second(datetime)/epoch_length)) %>%
                                mutate(epoch_id = time_minute + seconds(second)) %>%
                                select(-Timestamp, -`Accelerometer X`, -`Accelerometer Y`,-`Accelerometer Z`,
                                       -time_minute, -second) %>%
                                data.table() )
                       })
      tmp1[[floor(nrow(dt) / 1e5) + 1]] <- dt[((floor(nrow(dt) / 1e5) * 1e5) + 1):(nrow(dt))] %>% 
        mutate(device_id = device_id,
               epoch_id = paste0(substr(Timestamp, 1, 18), 0),
               datetime = Timestamp,
               vec.mag = sqrt(`Accelerometer X`^2 + `Accelerometer Y`^2 + `Accelerometer Z`^2)) %>%
        mutate(time_minute = floor_date(datetime, "minute"),
               second = epoch_length * floor(second(datetime)/epoch_length)) %>%
        mutate(epoch_id = time_minute + seconds(second)) %>%
        select(-Timestamp, -`Accelerometer X`, -`Accelerometer Y`,-`Accelerometer Z`,
               -time_minute, -second) %>%
        data.table() 
      
      rm(dt)                                        
      dt <- rbindlist(tmp1)
      rm(tmp1)
    } 
   
    data[[which(csv.files[str_detect(csv.files, "RAW.csv")] == i)]] <- dt
  }
  rm(dt)
  # collapse to single file
  data <- rbindlist(data)
  setkey(data, device_id, epoch_id)
  return(data)
}


