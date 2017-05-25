
# summarise test/training data
train.summary <- summarise.data(data = train.data[device_id != "TAS1E31150005"], 
                                human.devices = c("TAS1E31150003", "TAS1E31150028"))
## filter to walk period
train.summary <- train.summary[epoch_id >= ymd_hms("2016-05-04 18:30:00") & 
                                  epoch_id < ymd_hms("2016-05-04 18:55:00")]
cache("train.summary")


# summarise evaluation data
## same subjects as train
eval1.summary <- summarise.data(data = eval1.data, 
                                human.devices = c("TAS1E31150030", "TAS1E35150289"))
## filter to walk period
eval1.summary <- eval1.summary[epoch_id >= ymd_hms("2016-06-15 07:53:00") & 
                                 epoch_id < ymd_hms("2016-06-15 08:22:00")]
cache("eval1.summary")


# summarise maries evalusation data
eval2.summary <- summarise.data(data = eval2.data, human.devices = "TAS1E31150005")
## filter to walk period
eval2.summary <- eval2.summary[epoch_id > ymd_hms("20160614 064500") & 
                                 epoch_id < ymd_hms("20160614 070030")]
cache("eval2.summary")
