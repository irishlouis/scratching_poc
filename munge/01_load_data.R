# get csv files from /data

## load data for POC
### raw data
poc.raw.data <- load.data(datafolder = "POC", epoch_length = 10 )
cache("poc.raw.data")
### subject diary data
poc.subj.diary <- fread("")
