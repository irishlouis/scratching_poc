h2oLocal <- h2o.init(n = -1)

model_data <- poc.summary %>% 
  select(-device_id, -epoch_id) 

set.seed(2532)
s <- createDataPartition(model_data$scratch_event, p = 0.6, list = F)

## using h2o / sparkling water
# convert to h20_frame (uses the same underlying rdd)
training_hex <- as.h2o(model_data[s])
test_hex     <- as.h2o(model_data[-s])

# label string needs to be categorical
training_hex$scratch_event <- as.factor(training_hex$scratch_event)
test_hex$scratch_event     <- as.factor(test_hex$scratch_event)

label <- "scratch_event"
preds <- setdiff(names(training_hex), label)

rf_model  <- h2o.randomForest(x = preds,
                     y = label,
                     training_frame = training_hex,
                     ntrees = 50, 
                     max_depth = 10,
                     stopping_metric = "misclassification",
                     stopping_rounds = 2,
                     stopping_tolerance = .01, 
                     balance_classes = TRUE)

gbm_model <- h2o.gbm(x = preds,
                     y = label,
                     training_frame = training_hex,
                     ntrees = 100, 
                     max_depth = 10,
                     stopping_metric = "misclassification",
                     stopping_rounds = 2,
                     stopping_tolerance = .01, 
                     balance_classes = TRUE)

dnn_model  <- h2o.deeplearning(x = preds, 
                              y = label,
                              training_frame = training_hex,
                              activation = "Rectifier",
                              hidden = c(200,200,200),
                              epochs = 10000,
                              stopping_metric = "misclassification",
                              stopping_rounds = 2,
                              stopping_tolerance = .01, 
                              standardize = TRUE, 
                              balance_classes = TRUE)

confusionMatrix(test_hex$scratch_event %>% as.vector, 
                h2o.predict(rf_model,  test_hex)$predict %>% as.vector)


confusionMatrix(test_hex$scratch_event %>% as.vector, 
                h2o.predict(gbm_model, test_hex)$predict %>% as.vector)
h2o.confusionMatrix(gbm_model, test_hex)
h2o.saveModel(gbm_model, path = "gbm_model")

confusionMatrix(test_hex$scratch_event %>% as.vector, 
                h2o.predict(dnn_model, test_hex)$predict %>% as.vector)


h2o::h2o.shutdown(prompt = F)
