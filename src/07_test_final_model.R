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
