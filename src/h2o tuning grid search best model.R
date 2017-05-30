
install.packages(c(
  #showcase required packages
  "data.table",
  "pROC",
  "bit64",
  "logging"))

#H2O deps
for (pkg in c("methods","statmod","stats","graphics","RCurl","jsonlite","tools","utils")) {
  if (!(pkg %in% rownames(installed.packages()))) { install.packages(pkg) }
}

install.packages("h2o", 
                 type = "source", 
                 repos = (c("http://h2o-release.s3.amazonaws.com/h2o/rel-turing/6/R")))


find_best_model <- function(...) {
  h2o_grid <- h2o.grid(...)
  
  best_model <- NULL
  for (model_id in h2o_grid@model_ids) {
    model <- h2o.getModel(model_id)
    model_auc <- h2o.auc(model, xval = TRUE)
    print(sprintf("Model %s got %s AUC", model@model_id, model_auc))
    if (is.null(best_model)) {
      best_model <- list(
        model = model,
        threshold = h2o.find_threshold_by_max_metric(h2o.performance(model, xval = TRUE), 
                                                     "min_per_class_accuracy"),
        thresholds = h2o.performance(model, xval = TRUE))
    } else if (h2o.auc(best_model$model, xval = TRUE) < model_auc) {
      best_model <- list(
        model = model,
        threshold = h2o.find_threshold_by_max_metric(h2o.performance(model, xval = TRUE), 
                                                     "min_per_class_accuracy"),
        thresholds = h2o.performance(model, xval = TRUE))
    }
  }
  return(best_model)
}



library(data.table)
library(h2o)
library(bit64)
library(pROC)
library(logging)


logging::basicConfig()

loginfo("--> Loading data...")
all_data <- fread("https://raw.githubusercontent.com/WLOGSolutions/telco-customer-churn-in-r-and-h2o/master/data/edw_cdr.csv")
all_data <- all_data[, !c("month", "year"), with = FALSE]
all_data <- all_data[complete.cases(all_data)]
all_data <- all_data[!duplicated(all_data)]

loginfo("--> loaded %s rows", nrow(all_data))

set.seed(1234)

loginfo("--> Cleaning&transforming data...")
all_data[, ind := factor(sample(0:1, size = .N, replace = TRUE, prob = c(0.3, 0.7)),
                         levels = 0:1,
                         labels = c("Test", "Train"))]
all_data[, churn := factor(ifelse(churn == 1, "churn", "nochurn"))]

loginfo("--> done")

h2o_local <- h2o.init(nthreads = -1, 
                      max_mem_size = "2g")
h2o.removeAll()

h2o_train <- as.h2o(x = all_data[ind == "Train"],
                    destination_frame = "churn_train")

h2o_test <- as.h2o(x = all_data[ind == "Test"],
                   destination_frame = "churn_test")

loginfo("--> Datasets imported into H2O cluster")

predictors <- setdiff(colnames(all_data),
                      c("churn",
                        "customerid"))
churn_var <- "churn"
best_model <- find_best_model(algorithm = "gbm",
                              grid_id = "gbm_grid",
                              training_frame = h2o_train,
                              x = predictors,
                              y = churn_var,
                              nfolds = 5,
                              balance_classes = TRUE,
                              distribution  = "bernoulli",
                              hyper_params = list(
                                ntrees = c(50, 
                                           100#, 
                                           #500
                                ),
                                max_depth = c(4,
                                              8#,
                                              #16,
                                              #32
                                )))

h2o.saveModel(best_model$model, path = "export", force = TRUE)

loginfo("--> Best model exported into export folder")

loginfo("--> Best model with AUC=%s", h2o.auc(best_model$model, xval = TRUE))
loginfo("--> Threshold for min per class accuracy metric = %s", best_model$threshold)

test_preds <- cbind(all_data[ind == "Test"], 
                    as.data.table(h2o.predict(object = best_model$model, 
                                              newdata = h2o_test))[, .(churn_pred = ifelse(churn > 1 - best_model$threshold, 
                                                                                           "churn", 
                                                                                           "nochurn"), 
                                                                       churn_prob = churn, 
                                                                       org_predict = predict)])
# doesn't look like using threshold improves performance??
caret::confusionMatrix(test_preds$churn_pred,  test_preds$churn)
caret::confusionMatrix(test_preds$org_predict, test_preds$churn)

loginfo("--> Scoring test datasets done")

loginfo("Test accuracy: %.3f", test_preds[, mean(churn == churn_pred)])

loginfo("Test precision: %.3f", test_preds[, sum(churn == "churn" & churn_pred == "churn")/(sum(churn == "churn" & churn_pred == "churn") +
                                                                                              sum(churn == "nochurn" & churn_pred == "churn"))])
loginfo("Test recall: %.3f", test_preds[, sum(churn == "churn" & churn_pred == "churn")/(sum(churn == "churn" & churn_pred == "churn") +
                                                                                           sum(churn == "churn" & churn_pred == "nochurn"))])

tree_roc <- pROC::roc(test_preds[, churn], test_preds[, churn_prob])
loginfo("Test AUC: %.3f", pROC::auc(tree_roc))


h2o.shutdown()
