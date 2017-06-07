rm(poc.raw.data)

# partition data
set.seed(6354987)
s <- createDataPartition(poc.summary$scratch_event, p = 0.7, list = FALSE)
training <- poc.summary[s][,':='(
  device_id = NULL,
  epoch_id = NULL)]
testing <- poc.summary[-s][,':='(
  device_id = NULL,
  epoch_id = NULL)]

######################################################################################

# bog standard method
ctrl <- trainControl(
  method='repeatedcv',
  repeats=5
)
set.seed(456)
nnet_default <- train(scratch_event ~.,
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
nnet_random <- train(scratch_event ~.,
              method = "nnet",
              metric = "Kappa",
              data = training, 
              trControl = ctrl1, 
              tuneLength = 20,
              trace = FALSE)


######################################################################################
# give model specific tuning grid - SLOW
ctrl2 <- trainControl(
  method='repeatedcv',
  repeats= 5
)
tune_grid <- expand.grid(size = c(1, 5, 8), 
                         decay = seq(0, .2, .02))

# this gets very big very quickly
message("note no. of models = ", nrow(tune_grid), " x number of CV (5)")

set.seed(456)
nnet_grid <- train(scratch_event ~.,
              method = "nnet",
              metric = "Kappa",
              data = training, 
              trControl = ctrl2, 
              tuneGrid = tune_grid,
              trace = FALSE)

# compare to previous model
compare_models(nnet_default, nnet_random)
compare_models(nnet_default, nnet_grid)
compare_models(nnet_random, nnet_grid)

# cache models
lapply(c("nnet_default", "nnet_random", "nnet_grid"), cache)

# compare results on testing
confusionMatrix(predict(nnet_default,  testing), testing$scratch_event)
confusionMatrix(predict(nnet_random, testing), testing$scratch_event)
confusionMatrix(predict(nnet_grid, testing), testing$scratch_event)

# looks like default model does the best
## size = 1 and decay = 0.1

## train full final model on all data using these settings
tune_grid <- expand.grid(size = c(1), 
                         decay = c(0.1))

# this gets very big very quickly
message("note no. of models = ", nrow(tune_grid), " x number of CV (5)")

set.seed(456)
nnet_final <- train(scratch_event ~.,
                   method = "nnet",
                   metric = "Kappa",
                   data = poc.summary %>% select(-device_id, -epoch_id), 
                   tuneGrid = tune_grid,
                   trace = FALSE)
summary(nnet_final)
cache("nnet_final")




