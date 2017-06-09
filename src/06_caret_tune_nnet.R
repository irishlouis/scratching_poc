rm(poc.raw.data)

# partition data
set.seed(6354987)
s <- createDataPartition(zscale.poc.summary$scratch_event, p = 0.7, list = FALSE)
training <- zscale.poc.summary[s][,':='(
  device_id = NULL,
  epoch_id = NULL)]
testing <- zscale.poc.summary[-s][,':='(
  device_id = NULL,
  epoch_id = NULL)]

######################################################################################

# bog standard method
ctrl <- trainControl(
  method='repeatedcv',
  repeats=5
)
set.seed(456)
mdl1 <- train(scratch_event ~.,
             method = "nnet",
             metric = "Kappa",
             data = training,
             trace = FALSE,
             trControl = ctrl)

######################################################################################
# expanding the search for opt tuning parameters, but randomly
ctrl1 <- trainControl(
  method='repeatedcv',
  repeats=5,
  search="random"
)
set.seed(456)
mdl2 <- train(scratch_event ~.,
              method = "nnet",
              metric = "Kappa",
              data = training, 
              trControl = ctrl1, 
              tuneLength = 20,
              trace = FALSE)

# compare to original model
## mdl1 is little bit better not sig
compare_models(mdl1, mdl2)

######################################################################################
# give model specific tuning grid - SLOW

ctrl2 <- trainControl(
  method='repeatedcv',
  repeats= 5
)
nnet_grid <- expand.grid(size = c(10, 15, 20), 
                         decay = seq(0, .2, .02))

# this gets very big very quickly
message("note no. of models = ", nrow(nnet_grid), " x number of CV (5)")

set.seed(456)
mdl3 <- train(scratch_event ~.,
              method = "nnet",
              metric = "Kappa",
              data = training, 
              trControl = ctrl2, 
              tuneGrid = nnet_grid,
              trace = FALSE)

# compare to previous model
compare_models(mdl1, mdl3)
compare_models(mdl2, mdl3)

# compare results on testing
confusionMatrix(predict(mdl1, testing), testing$scratch_event)
confusionMatrix(predict(mdl2, testing), testing$scratch_event)
confusionMatrix(predict(mdl3, testing), testing$scratch_event)

# mdl1 is best 
## train final model on all data using these setting

final_grid <- expand.grid(size = 1, 
                          decay = 0.1)
caret_nnet_mdl <- train(scratch_event ~.,
                        method = "nnet",
                        metric = "Kappa",
                        data = zscale.poc.summary %>% select(-device_id, -epoch_id), 
                        trControl = ctrl2, 
                        tuneGrid = final_grid,
                        trace = FALSE)

# check against original model
compare_models(mdl1, caret_nnet_mdl, metric = "Kappa")

confusionMatrix(predict(mdl1, testing), testing$scratch_event)
confusionMatrix(predict(caret_nnet_mdl, testing), testing$scratch_event)

# cache the final model
cache("caret_nnet_mdl")

# housekeeping
rm(training)
rm(testing)
rm(mdl1)
rm(mdl2)
rm(mdl3)
rm(final_grid)
rm(nnet_grid)
rm(ctrl)
rm(ctrl1)
rm(ctrl2)
