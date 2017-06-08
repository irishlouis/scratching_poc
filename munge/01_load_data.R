# get csv files from /data

## load data for POC
### raw data
poc.raw.data <- load.data(datafolder = "POC", epoch_length = 10 )
cache("poc.raw.data")

## load test data for same subj as POC model but different dates
test1.raw.data <- load.data(datafolder = "test1_data", epoch_length = 10)
cache("test1.raw.data")

## load test data for unseen subj 
test2.raw.data <- load.data(datafolder = "test2_data", epoch_length = 10)
cache("test2.raw.data")
