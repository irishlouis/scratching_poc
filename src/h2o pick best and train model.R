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

# Deeplearning Hyperparamters
activation_opt <- c("Rectifier", "RectifierWithDropout") #  ,"Maxout", "MaxoutWithDropout") 
hidden_opt <- list(c(5), c(10), c(50), c(100), c(10,10), c(20,15), c(50,50,50), 
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

###############################################################

# eval models on test dataset
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
                              activation = "RectifierWithDropout", 
                              hidden = c(200, 200), 
                              l1 = 0.0, 
                              l2 = 0.001)
summary(poc_model)
h2o.performance(poc_model, all_data)


confusionMatrix(ifelse(h2o.predict(poc_model, all_data)$scratch %>% as.vector() > f1_threshold$threshold,
                       "scratch", "not_scratch"),
                all_data[, y] %>% as.vector)



h2o.saveModel(poc_model, path = "ml_models/")


unseen_data <- as.h2o(test2.summary %>% select(-device_id, -epoch_id))

unseen_h2o_result <- h2o.predict(poc_model, unseen_data)
table(unseen_h2o_result$predict %>% as.vector)

data.frame(epoch_id = test2.summary$epoch_id, pred = unseen_h2o_result$predict %>% as.vector) %>%
  filter(pred == "scratch")

h2o::h2o.removeAll()
h2o::h2o.shutdown(prompt = FALSE)
