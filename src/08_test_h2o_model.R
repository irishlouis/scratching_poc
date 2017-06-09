# init local h2o
h2oLocal <-  h2o.init(nthreads = -1, max_mem_size = "15G")  

# load models
best_gbm <- h2o::h2o.loadModel("ml_models/Grid_GBM_RTMP_sid_aee7_5_model_R_1496865147756_1_model_2")
best_rf  <- h2o::h2o.loadModel("ml_models/Grid_DRF_RTMP_sid_aee7_5_model_R_1496865147756_27136_model_16")
best_glm <- h2o::h2o.loadModel("ml_models/Grid_GLM_RTMP_sid_aee7_5_model_R_1496865147756_58078_model_7")  

# create test / train h2o files
test1 <- as.h2o(zscale.test1.summary %>% select(-device_id, -epoch_id))
test2 <- as.h2o(zscale.test2.summary %>% select(-device_id, -epoch_id))

test1_gbm <- as.data.frame(h2o.predict(best_gbm, test1))
test1_rf <- as.data.frame(h2o.predict(best_rf, test1))
test1_glm <- as.data.frame(h2o.predict(best_glm, test1))

test2_gbm <- as.data.frame(h2o.predict(best_gbm, test2))
test2_rf <- as.data.frame(h2o.predict(best_rf, test2))
test2_glm <- as.data.frame(h2o.predict(best_glm, test2))

cache("test1_gbm")
cache("test1_rf")
cache("test1_glm")

cache("test2_gbm")
cache("test2_rf")
cache("test2_glm")

h2o::h2o.removeAll()
h2o::h2o.shutdown(prompt = FALSE)
