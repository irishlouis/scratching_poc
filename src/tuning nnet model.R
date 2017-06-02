require(caret) 
require(dplyr)
require(ISLR) # use Auto dataset

dt <- Auto %>% mutate(cyl = as.factor(cylinders)) %>% select(-origin, -name, -cylinders)

head(dt)

# bog standard method
ctrl <- trainControl(
  method='repeatedcv',
  repeats=5
)
sed.seed(456)
mdl <- train(cyl ~.,
             method = "nnet",
             metric = "Kappa",
             data = dt,
             trControl = ctrl)

######################################################################################
# expanding the search for opt tuning parameters, but randomly
ctrl1 <- trainControl(
  method='repeatedcv',
  repeats=5,
  search="random"
)
set.seed(456)
mdl1 <- train(cyl ~.,
              method = "nnet",
              metric = "Kappa",
              data = dt, 
              trControl = ctrl1, 
              tuneLength = 10)

# compare to original model
## new model is little bit better
compare_models(mdl, mdl1)

######################################################################################
# give model specific tuning grid
ctrl2 <- trainControl(
  method='repeatedcv',
  repeats=5
)
nnet_grid <- expand.grid(size = c(1, 3, 6), 
                         decay = seq(.3, .4, .01))

message("note no. of models = ", nrow(nnet_grid), " x number of CV (5)")
# this gets very big very quickly, have narrowed grid here to area of interest

set.seed(456)
mdl2 <- train(cyl ~.,
              method = "nnet",
              metric = "Kappa",
              data = dt, 
              trControl = ctrl2, 
              tuneGrid = nnet_grid)

# compare to original model
compare_models(mdl, mdl2)

######################################################################################
# can we improve model performance with tuning paramters
require(rBayesianOptimization)
ctrl3 <- trainControl(method = "repeatedcv", repeats = 5)

# function to opt model
## returns what is to be maximised (I'm using Kappa) - if looking to minimised something (RMSE) return -ive
nnet.fit.bayes <- function(size, decay) {
  size <- round(size, 0)
  txt <- capture.output(
    mod <- train(cyl ~ ., data = dt,
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
  size = c(size = 1, size = 15),
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
mdl3 <- train(cyl ~ ., 
              data = dt,
              method = "nnet",
              tuneGrid = data.frame(decay = ba_search$Best_Par["decay"],
                                    size = round(ba_search$Best_Par["size"], 0)),
              metric = "Kappa",
              trControl = ctrl3)

# compare to original model
compare_models(mdl, mdl3)

