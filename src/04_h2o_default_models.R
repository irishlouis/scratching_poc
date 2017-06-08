# init local h2o cluster
h2oLocal <- h2o.init(n = -1)

set.seed(2532)
s <- createDataPartition(zscale.poc.summary$scratch_event, p = 0.7, list = F)

## using h2o / sparkling water
# convert to h20_frame (uses the same underlying rdd)
training_hex <- as.h2o(zscale.poc.summary[s] %>% 
                         select(-device_id, -epoch_id))
test_hex     <- as.h2o(zscale.poc.summary[-s] %>% 
                         select(-device_id, -epoch_id))

# label string needs to be categorical
training_hex$scratch_event <- as.factor(training_hex$scratch_event)
test_hex$scratch_event     <- as.factor(test_hex$scratch_event)

label <- "scratch_event"
preds <- setdiff(names(training_hex), label)

h2o_rf_default  <- h2o.randomForest(model_id = "h2o_rf_default",
                     x = preds,
                     y = label,
                     training_frame = training_hex,
                     ntrees = 50, 
                     max_depth = 10, 
                     balance_classes = TRUE)

h2o_gbm_default <- h2o.gbm(model_id = "h2o_gbm_default",x = preds,
                     y = label,
                     training_frame = training_hex,
                     ntrees = 100, 
                     max_depth = 10, 
                     balance_classes = TRUE)

h2o_dnn_default  <- h2o.deeplearning(model_id = "h2o_dnn_default",
                              y = label,
                              training_frame = training_hex,
                              activation = "Rectifier",
                              hidden = c(200,200),
                              epochs = 500,
                              balance_classes = TRUE)

h2o.confusionMatrix(h2o_rf_model, test_hex)
h2o.confusionMatrix(h2o_gbm_model, test_hex)
h2o.confusionMatrix(h2o_dnn_model, test_hex)

h2o.performance(h2o_rf_model, test_hex)
h2o.performance(h2o_gbm_model, test_hex)
h2o.performance(h2o_dnn_model, test_hex)

h2o::h2o.shutdown(prompt = F)
