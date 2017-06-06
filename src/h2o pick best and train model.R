# free some memory
rm(poc.raw.data)

# init local h2o
localH2O <-  h2o.init(nthreads = -1)  

# partition data 
set.seed(987654)
s <- createDataPartition(poc.summary$scratch_event, p = 0.7, list = FALSE)

# create test / train h2o files
train <- as.h2o(poc.summary[s] %>% select(-device_id, -epoch_id))
test  <- as.h2o(poc.summary[-s] %>% select(-device_id, -epoch_id))

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
                        max_runtime_secs = 1200)
nfolds <- 5

# GBM Hyperparamters
learn_rate_opt <- c(0.01, 0.03) 
max_depth_opt <- c(3, 4, 5, 6, 9)
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
                     fold_assignment = "Modulo",
                     keep_cross_validation_predictions = TRUE,
                     hyper_params = hyper_params,
                     search_criteria = search_criteria)

h2o.getGrid(gbm_grid@grid_id, sort_by="AUC", decreasing = T)
gbm_models <- lapply(gbm_grid@model_ids, function(model_id) h2o.getModel(model_id))



# RF Hyperparamters
## won't converge for some reason TBC
# mtries_opt <- 8:20 
# max_depth_opt <- c(5, 10, 15, 20, 25)
# sample_rate_opt <- c(0.7, 0.8, 0.9, 1.0)
# col_sample_rate_per_tree_opt <- c(0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8)
# hyper_params <- list(mtries = mtries_opt,
#                      max_depth = max_depth_opt,
#                      sample_rate = sample_rate_opt,
#                      col_sample_rate_per_tree = col_sample_rate_per_tree_opt)
# 
# rf_grid <- h2o.grid("randomForest", x = x, y = y,
#                     training_frame = train,
#                     ntrees = 200,
#                     seed = 1,
#                     nfolds = nfolds,
#                     fold_assignment = "Modulo",
#                     keep_cross_validation_predictions = TRUE,                    
#                     hyper_params = hyper_params,
#                     search_criteria = search_criteria)
# h2o.getGrid(rf_grid@grid_id, sort_by="AUC", decreasing = T)
# rf_models <- lapply(rf_grid@model_ids, function(model_id) h2o.getModel(model_id))



# Deeplearning Hyperparamters
activation_opt <- c("Rectifier", "RectifierWithDropout") #  ,"Maxout", "MaxoutWithDropout") 
hidden_opt <- list(c(10), c(50), c(100), c(10,10), c(20,15), c(50,50,50), 
                   c(100, 100),  c(200,200), c(100, 100, 100), c(200, 200, 200))
l1_opt <- c(0, 1e-3, 1e-5)
l2_opt <- c(0, 1e-3, 1e-5)
hyper_params <- list(activation = activation_opt,
                     hidden = hidden_opt,
                     l1 = l1_opt,
                     l2 = l2_opt)

dl_grid <- h2o.grid("deeplearning", x = x, y = y,
                    training_frame = train,
                    epochs = 15,
                    seed = 1,
                    nfolds = nfolds, 
                    fold_assignment = "Modulo",
                    keep_cross_validation_predictions = TRUE,                    
                    variable_importances = TRUE,
                    balance_classes = TRUE,
                    hyper_params = hyper_params,
                    search_criteria = search_criteria)
h2o.getGrid(dl_grid@grid_id, sort_by="AUC", decreasing = T)
dl_models <- lapply(dl_grid@model_ids, function(model_id) h2o.getModel(model_id))


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
                     keep_cross_validation_predictions = TRUE,                    
                     hyper_params = hyper_params,
                     search_criteria = search_criteria)
h2o.getGrid(glm_grid@grid_id, sort_by="AUC", decreasing = T)
glm_models <- lapply(glm_grid@model_ids, function(model_id) h2o.getModel(model_id))

###############################################################

# eval models on test dataset

gbm_grid <- h2o.getGrid(gbm_grid@grid_id, sort_by="AUC", decreasing = T)
gbm_models <- lapply(gbm_grid@model_ids, function(model_id) h2o.getModel(model_id))
h2o.confusionMatrix(gbm_models[[1]], test)
h2o.varimp(gbm_models[[1]])
gbm_grid <- h2o.getGrid(gbm_grid@grid_id, sort_by="logloss", decreasing = F)
gbm_models <- lapply(gbm_grid@model_ids, function(model_id) h2o.getModel(model_id))
h2o.confusionMatrix(gbm_models[[1]], test)
h2o.varimp(gbm_models[[1]])

dl_grid <- h2o.getGrid(dl_grid@grid_id, sort_by="AUC", decreasing = T)
dl_models <- lapply(dl_grid@model_ids, function(model_id) h2o.getModel(model_id))
h2o.confusionMatrix(dl_models[[1]], test)
h2o.performance(dl_models[[1]], test)
h2o.varimp(dl_models[[1]])

dl_grid <- h2o.getGrid(dl_grid@grid_id, sort_by="logloss", decreasing = F)
dl_models <- lapply(dl_grid@model_ids, function(model_id) h2o.getModel(model_id))
h2o.confusionMatrix(dl_models[[1]], test)
h2o.performance(dl_models[[1]], test)
h2o.varimp(dl_models[[1]])

# need to capture F1 threshold criteria for future predictions 
f1_threshold <- h2o.performance(dl_models[[1]], test)@metrics$max_criteria_and_metric_scores[1,]

confusionMatrix(ifelse(h2o.predict(dl_models[[1]], test)$scratch %>% as.vector() > f1_threshold$threshold,
                       "scratch", "not_scratch"),
                test[, y] %>% as.vector)

# visualise
r <- ifelse(h2o.predict(dl_models[[1]], test)$scratch %>% as.vector() > f1_threshold$threshold,
       "scratch", "not_scratch")

poc.summary[-s] %>% 
  mutate(pred_result = r) %>%
  filter(pred_result != scratch_event) %>% 
  select(epoch_id) %>% as.vector()

poc.summary[-s] %>% 
  mutate(pred_result = r) %>% 
  select(epoch_id, scratch_event, pred_result) %>%
  melt(id.vars = "epoch_id") %>%
  ggplot(aes(epoch_id, value)) + 
    geom_point(aes(col = value, shape = variable)) +
    facet_wrap(~variable, nrow = 2)

# same model is best in both instances
## activation = Rectifier hidden = [200, 200] l1 = 0.0 l2 = 0.001

glm_grid <- h2o.getGrid(glm_grid@grid_id, sort_by="AUC", decreasing = T)
glm_models <- lapply(glm_grid@model_ids, function(model_id) h2o.getModel(model_id))
h2o.confusionMatrix(glm_models[[1]], test)
glm_grid <- h2o.getGrid(glm_grid@grid_id, sort_by="logloss", decreasing = F)
glm_models <- lapply(glm_grid@model_ids, function(model_id) h2o.getModel(model_id))
h2o.confusionMatrix(glm_models[[1]], test)
h2o.varimp(glm_models[[1]])

# save best model - dnn
h2o.saveModel(dl_models[[1]], path = "ml_models/")

# train model on all data for further testing
all_data <- as.h2o(poc.summary %>% select(-device_id, -epoch_id))
all_data[,y] <- as.factor(all_data[,y])  

poc_model <- h2o.deeplearning("poc_dnn_model", 
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


confusionMatrix(ifelse(h2o.predict(poc_model, all_data)$scratch %>% as.vector() > f1_threshold$threshold,
                       "scratch", "not_scratch"),
                all_data[, y] %>% as.vector)



h2o.saveModel(poc_model, path = "ml_models/")

h2o::h2o.removeAll()
h2o::h2o.shutdown(prompt = FALSE)
