# init local h2o
h2oLocal <-  h2o.init(nthreads = -1, max_mem_size = "15G")  

# partition data 
set.seed(987654)
s <- createDataPartition(zscale.poc.summary$scratch_event, p = 0.7, list = FALSE)

# create test / train h2o files
train <- as.h2o(zscale.poc.summary[s] %>% select(-device_id, -epoch_id))
test  <- as.h2o(zscale.poc.summary[-s] %>% select(-device_id, -epoch_id))

y <- "scratch_event"
x <- setdiff(names(train), y)

#For binary classification, response should be a factor
train[,y] <- as.factor(train[,y])  
test[,y]  <- as.factor(test[,y])

# Random Grid Search (e.g. 120 second maximum)
# This is set to run fairly quickly, increase max_runtime_secs 
# or max_models to cover more of the hyperparameter space.
# Also, you can expand the hyperparameter space of each of the 
# algorithms by modifying the hyper param code below.

search_criteria <- list(strategy = "RandomDiscrete", 
                        max_runtime_secs = 600)
nfolds <- 5

#############################################################################################
# GBM Hyperparamters
learn_rate_opt <- c(0.01, 0.03) 
max_depth_opt <- c(3, 5, 6, 9, 10)
sample_rate_opt <- c(0.7, 0.8, 0.9, 1.0)
col_sample_rate_opt <- c(0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8)
hyper_params <- list(learn_rate = learn_rate_opt,
                     max_depth = max_depth_opt, 
                     sample_rate = sample_rate_opt,
                     col_sample_rate = col_sample_rate_opt)

gbm_grid <- h2o.grid("gbm", x = x, y = y,
                     training_frame = train,
                     ntrees = 100,
                     seed = 1,
                     nfolds = nfolds,
                     balance_classes = TRUE,
                     fold_assignment = "Modulo",
                     keep_cross_validation_predictions = TRUE,
                     hyper_params = hyper_params,
                     search_criteria = search_criteria)

## save best model
gbm_grid_auc <- h2o.getGrid(gbm_grid@grid_id, sort_by="AUC", decreasing = T)
gbm_models <- lapply(gbm_grid_auc@model_ids, function(model_id) h2o.getModel(model_id))
best_gbm <- gbm_models[[1]]
h2o.saveModel(best_gbm, path = "ml_models/")
rm(gbm_grid_auc)
rm(gbm_models)
rm(learn_rate_opt)
rm(max_depth_opt)
rm(sample_rate_opt)
rm(col_sample_rate_opt)
rm(hyper_params)

##############################################################################################
# RF Hyperparamters
## won't converge for some reason TBC
max_depth_opt <- c(5, 10, 15, 20, 25)
sample_rate_opt <- c(0.7, 0.8, 0.9, 1.0)
col_sample_rate_per_tree_opt <- c(0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8)
hyper_params <- list(max_depth = max_depth_opt,
                     sample_rate = sample_rate_opt,
                     col_sample_rate_per_tree = col_sample_rate_per_tree_opt)

rf_grid <- h2o.grid("randomForest", x = x, y = y,
                    training_frame = train,
                    ntrees = 200,
                    seed = 1,
                    nfolds = nfolds,
                    fold_assignment = "Modulo",
                    balance_classes = TRUE,
                    keep_cross_validation_predictions = TRUE,
                    hyper_params = hyper_params,
                    search_criteria = search_criteria)

## save best model
rf_grid_auc <- h2o.getGrid(rf_grid@grid_id, sort_by="AUC", decreasing = T)
rf_models <- lapply(rf_grid_auc@model_ids, function(model_id) h2o.getModel(model_id))
best_rf <- rf_models[[1]]
h2o.saveModel(best_rf, path = "ml_models/")
rm(rf_grid_auc)
rm(rf_models)
rm(max_depth_opt)
rm(sample_rate_opt)
rm(col_sample_rate_per_tree_opt)
rm(hyper_params)

##############################################################################################
# Deeplearning Hyperparamters
activation_opt <- c("Rectifier", "RectifierWithDropout") 
hidden_opt <- list(c(10), c(50), c(100), c(10,10), c(20,15), c(50,50), 
                   c(100, 100),  c(200,200), c(100, 100, 100))
l1_opt <- c(0, 1e-3, 1e-5)
l2_opt <- c(0, 1e-3, 1e-5)
hyper_params <- list(activation = activation_opt,
                     hidden = hidden_opt,
                     l1 = l1_opt,
                     l2 = l2_opt)

dl_grid <- h2o.grid("deeplearning", x = x, y = y,
                    training_frame = train,
                    epochs = 10,
                    seed = 1,
                    nfolds = nfolds, 
                    fold_assignment = "Modulo",
                    keep_cross_validation_predictions = TRUE,                    
                    balance_classes = TRUE,
                    hyper_params = hyper_params,
                    search_criteria = search_criteria)

## save best model
dl_grid_auc <- h2o.getGrid(dl_grid@grid_id, sort_by="AUC", decreasing = T)
dl_models <- lapply(dl_grid_auc@model_ids, function(model_id) h2o.getModel(model_id))
best_dl <- dl_models[[1]]
h2o.saveModel(best_dl, path = "ml_models/")
rm(dl_grid_auc)
rm(dl_models)
rm(activation_opt)
rm(hidden_opt) 
rm(l1_opt)
rm(l2_opt)
rm(hyper_params)

###############################################################################################
# GLM Hyperparamters
alpha_opt <- seq(0,1,0.1)
lambda_opt <- c(0,1e-7,1e-5,1e-3,1e-1)
hyper_params <- list(alpha = alpha_opt,
                     lambda = lambda_opt)

glm_grid <- h2o.grid("glm", x = x, y = y,
                     training_frame = train,
                     family = "binomial",
                     nfolds = nfolds,
                     fold_assignment = "Modulo",
                     balance_classes = TRUE,
                     keep_cross_validation_predictions = TRUE,                    
                     hyper_params = hyper_params,
                     search_criteria = search_criteria)

## save best model
glm_grid_auc <- h2o.getGrid(glm_grid@grid_id, sort_by="AUC", decreasing = T)
glm_models <- lapply(glm_grid_auc@model_ids, function(model_id) h2o.getModel(model_id))
best_glm <- glm_models[[1]]
h2o.saveModel(best_glm, path = "ml_models/")
rm(glm_grid_auc)
rm(glm_models)
rm(alpha_opt)
rm(lambda_opt)
rm(hyper_params)

##############################################################################################
# eval models on test dataset
h2o.confusionMatrix(best_gbm, test)
h2o.performance(best_gbm, test)

h2o.confusionMatrix(best_rf, test)
h2o.performance(best_rf, test)

h2o.confusionMatrix(best_dl, test)
h2o.performance(best_dl, test)

h2o.confusionMatrix(best_glm, test)
h2o.performance(best_glm, test)

##############################################################################################

# need to capture F1 threshold criteria for future predictions 
f1_threshold <- h2o.performance(dl_models[[1]], test)@metrics$max_criteria_and_metric_scores[1,]

confusionMatrix(ifelse(h2o.predict(dl_models[[1]], test)$scratch %>% as.vector() > f1_threshold$threshold,
                       "scratch", "not_scratch"),
                test[, y] %>% as.vector)


##########################################################################################
# retrain best model using all POC data

# train model on all data for further testing
all_data <- as.h2o(poc.summary %>% select(-device_id, -epoch_id))
all_data[,y] <- as.factor(all_data[,y])  

best_h2o_model <- h2o.deeplearning("best_h2o_model", 
                              x = x, y = y,
                              training_frame = all_data,
                              epochs = 15,
                              seed = 1,
                              nfolds = nfolds,
                              fold_assignment = "Modulo",
                              variable_importances = TRUE,
                              balance_classes = TRUE,
                              keep_cross_validation_predictions = TRUE,
                              activation = "Rectifier", 
                              hidden = c(200, 200, 200), 
                              l1 = 0.0, 
                              l2 = 0.001)
summary(poc_model)
h2o.performance(poc_model, all_data)

h2o.saveModel(best_h2o_model, path = "ml_models/")

h2o::h2o.removeAll()
h2o::h2o.shutdown(prompt = FALSE)
