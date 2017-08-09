rm(data)
rm(test1.raw.data)
rm(summary)

# test on same subject different dates
test1.summary$prediction <- predict(caret_nnet_mdl, zscale.test1.summary)

# test on unseen subject
test2.summary$prediction <- predict(caret_nnet_mdl, zscale.test2.summary)

# look at predicted events
predicted_scratching_events <- dplyr::bind_rows(
  test1.summary %>% 
    mutate(prediction = predict(caret_nnet_mdl, zscale.test1.summary)) %>%
    filter(prediction == "scratch"),
  test2.summary %>% 
    mutate(prediction = predict(caret_nnet_mdl, zscale.test2.summary)) %>%
    filter(prediction == "scratch")) %>%
  arrange(device_id, epoch_id)

cache("predicted_scratching_events")

predicted_scratching_events$epoch_id[predicted_scratching_events$device_id == "TAS1E31150003"]

scratching.times <- sapply(c("2017-06-02 03:26:00 UTC", "2017-06-02 03:26:10 UTC", "2017-06-02 03:26:20 UTC", "2017-06-02 03:26:30 UTC", 
                             "2017-06-02 03:28:00 UTC", "2017-06-02 03:28:10 UTC", "2017-06-02 03:28:20 UTC", "2017-06-02 03:28:30 UTC", 
                             "2017-06-02 03:30:00 UTC", "2017-06-02 03:30:10 UTC", "2017-06-02 03:30:20 UTC", "2017-06-02 03:30:30 UTC", 
                             "2017-06-02 03:56:00 UTC", "2017-06-02 03:56:10 UTC", "2017-06-02 03:56:20 UTC", "2017-06-02 03:56:30 UTC", 
                             "2017-06-02 04:07:00 UTC", "2017-06-02 04:07:10 UTC", "2017-06-02 04:07:20 UTC", "2017-06-02 04:07:30 UTC", 
                             "2017-06-02 04:27:00 UTC", "2017-06-02 04:27:10 UTC", "2017-06-02 04:27:20 UTC", "2017-06-02 04:27:30 UTC", 
                             "2017-06-02 06:10:00 UTC", "2017-06-02 06:10:10 UTC", "2017-06-02 06:10:20 UTC", "2017-06-02 06:10:30 UTC"), ymd_hms)

test2.summary[epoch_id %in% scratching.times]

confusionMatrix(test2.summary$prediction, 
factor(sapply(test2.summary$epoch_id, function(x) ifelse( x %in% scratching.times,
"scratch", "not_scratch"))))
