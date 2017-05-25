# optimise best model

# create model.data, filtering out epochs with low steps
## drop steps - only want numeric
model.data <- train.summary %>% select(-steps)

set.seed(456456)
s <- createDataPartition(model.data$subj_type, p = 0.6, list = FALSE)
training <- model.data[s][,':='(
  device_id = NULL,
  epoch_id = NULL)]
testing <- model.data[-s][,':='(
  device_id = NULL,
  epoch_id = NULL)]

# define model training control
init_control <- trainControl(
  method='repeatedcv',
  repeats=5,
  search="random",
  savePredictions=TRUE,
  classProbs=TRUE
)


# train list of models using control spec'd above
set.seed(456798)
init_model <- train(subj_type ~ ., 
                    data = training, 
                    trControl = init_control,
                    metric = "Kappa",
                    method = "nnet",
                    tuneLength = 10)
# The final values used for the model were size = 5 and decay = 0.006772878
varImp(init_model)
plot(varImp(init_model))

# look at validation results 
confusionMatrix(predict(init_model, testing), testing$subj_type)
init_model
cache("init_model")

###############################################
#
# can we improve model performance with tuning paramters

# new simpler ctrl func
optimise_ctrl <- trainControl(method = "repeatedcv", repeats = 5)

# function to opt model
nnet.fit.bayes <- function(size, decay) {
  size <- round(size, 0)
  ## Use the same model code but for a single (C, sigma) pair. 
  txt <- capture.output(
    mod <- train(subj_type ~ ., data = training,
                 method = "nnet",
                 metric = "Kappa",
                 trControl = optimise_ctrl,
                 tuneGrid = data.frame(size = size, decay = decay))
  )
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
                                  n_iter = 50,
                                  acq = "ucb", 
                                  kappa = 1, 
                                  eps = 0.0,
                                  verbose = TRUE)
cache("ba_search")
#' Best Parameters Found: 
#' Round = 15	C = 123.7352	sigma = 0.7303	Value = 0.9896 

set.seed(456798)
final_model <- train(subj_type ~ ., 
                      data = training,
                      method = "nnet",
                      tuneGrid = data.frame(decay = ba_search$Best_Par["decay"],
                                            size = round(ba_search$Best_Par["size"], 0)),
                      metric = "Kappa",
                      trControl = optimise_ctrl)

# compare results for initial and tuned model
confusionMatrix(predict(final_model, testing), testing$subj_type)
confusionMatrix(predict(init_model, testing), testing$subj_type)

# no difference in performance 
compare_models(final_model, init_model)

# look at variable importance
varImp(init_model)
pdf("graphs/varImpSubjtypeInitMdl.pdf", compress = F)
plot(varImp(init_model))
dev.off()

varImp(final_model)
pdf("graphs/varImpSubjtypeFinalMdl.pdf", compress = F)
plot(varImp(final_model))
dev.off()

# plot neural network
pdf("graphs/subjtypeInitMdl.pdf", compress = F, width = 12)
plot.nnet(init_model)
dev.off()

pdf("graphs/subjtypeFinalMdl.pdf", compress = F, width = 12)
plot.nnet(final_model)
dev.off()
# chache
cache("final_model")
