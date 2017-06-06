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
mdl <- train(scratch_event ~.,
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
mdl1 <- train(scratch_event ~.,
              method = "nnet",
              metric = "Kappa",
              data = training, 
              trControl = ctrl1, 
              tuneLength = 10,
              trace = FALSE)

# compare to original model
## new model is little bit better
compare_models(mdl, mdl1)

######################################################################################
# give model specific tuning grid - SLOW
ctrl2 <- trainControl(
  method='repeatedcv',
  repeats= 5
)
nnet_grid <- expand.grid(size = c(1, 5, 8), 
                         decay = seq(0, .2, .02))

# this gets very big very quickly
message("note no. of models = ", nrow(nnet_grid), " x number of CV (5)")

set.seed(456)
mdl2 <- train(scratch_event ~.,
              method = "nnet",
              metric = "Kappa",
              data = training, 
              trControl = ctrl2, 
              tuneGrid = nnet_grid,
              trace = FALSE)

# compare to previous model
compare_models(mdl, mdl2)
compare_models(mdl1, mdl2)

# cache models
lapply(c("mdl", "mdl1", "mdl2"), cache)

# compare results on testing
confusionMatrix(predict(mdl,  testing), testing$scratch_event)
confusionMatrix(predict(mdl1, testing), testing$scratch_event)
confusionMatrix(predict(mdl2, testing), testing$scratch_event)





######################################################################################
# can we improve model performance with tuning paramters
require(rBayesianOptimization)
ctrl3 <- trainControl(method = "repeatedcv", 
                      repeats = 5)

# function to opt model
## returns what is to be maximised (I'm using Kappa) - if looking to minimised something (RMSE) return -ive
nnet.fit.bayes <- function(size, decay) {
  size <- round(size, 0)
  txt <- capture.output(
    mod <- train(scratch_event ~ ., 
                 data = training,
                 method = "nnet",
                 metric = "Kappa",
                 trControl = ctrl3,
                 tuneGrid = data.frame(size = size, decay = decay))
  )
  # don't care about Pred, so just return 0 value
  list(Score = getTrainPerf(mod)[, "TrainKappa"], Pred = 0)
}
# set bounds for search
bounds <- list(
  size = c(size = 1, size = 50),
  decay = c(decay = 0.0001, decay = .5))

# search for optimum model parameters
set.seed(8606)
ba_search <- BayesianOptimization(FUN = nnet.fit.bayes, 
                                  bounds = bounds,
                                  init_points = 5, 
                                  n_iter = 10,
                                  acq = "ucb", 
                                  kappa = 1, 
                                  eps = 0.0,
                                  verbose = TRUE)

# use best found values to train model
set.seed(456)
mdl3 <- train(scratch_event ~ ., 
              data = training,
              method = "nnet",
              tuneGrid = data.frame(decay = ba_search$Best_Par["decay"],
                                    size = round(ba_search$Best_Par["size"], 0)),
              metric = "Kappa",
              trControl = ctrl3)

# compare to previous model
compare_models(mdl, mdl3)
compare_models(mdl1, mdl3)
compare_models(mdl2, mdl3)