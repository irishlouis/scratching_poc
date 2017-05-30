



library(h2o)
library(dplyr)

train_dt  <- data.table::fread("c:/users/smithlou/documents/africa_soil/training.csv") %>% as.data.frame()
test_dt   <- data.table::fread("c:/users/smithlou/documents/africa_soil/sorted_test.csv") %>% as.data.frame()

# partition
set.seed(123)
s <- sample(1:nrow(train_dt), 0.8 * nrow(train_dt), replace = FALSE)

val_dt   <- train_dt[-s, ]
train_dt <- train_dt[s, ]

localH2O <- h2o.init(ip = "localhost", port = 54321, startH2O = TRUE, max_mem_size = "1g", nthreads = 6)
                          
## Import Data to H2O Cluster
train_hex <- as.h2o(train_dt)
val_hex   <- as.h2o( val_dt)
test_hex  <- as.h2o(test_dt)

dim(train_hex)
dim(val_hex)
dim(test_hex)

# bareboned DNN code

## One Variable at at Time
ls_label <- c("Ca", "P", "pH", "SOC", "Sand")

raw_sub1 <- data.frame(row_num = 1:nrow(test_hex))
raw_sub2 <- data.frame(row_num = 1:nrow(test_hex))
for (n_label in 1:5) {
  
  ## Display
  print(paste("Now training a DNN model for", ls_label[n_label]))
  
  ## Train a 50-node, three-hidden-layer Deep Neural Networks for 100 epochs
  model1 <- h2o.deeplearning(  x = 2:3595,             # column numbers for predictors
                               y = ls_label[n_label],  # column number for label
                               training_frame  = train_hex,
                               validation_frame = val_hex,
                               activation = "Rectifier",
                               hidden = c(50, 50, 50),
                               epochs = 100,
                               balance_classes = FALSE, 
                               classification_stop = -1)
  print("now gbm")
  ## Train a GBM for 100 epochs
  model2 <- h2o.gbm ( distribution = "AUTO", 
                      ignore_const_cols = TRUE,
                      x = 2:3595,            # column numbers for predictors
                      y = ls_label[n_label],  # column number for label
                      training_frame  = train_hex,
                      validation_frame = val_hex
                               )


  ## Use the model for prediction and store the results in submission template
  raw_sub1[, (n_label + 1)] <- as.matrix(h2o.predict(model1, test_hex))
  raw_sub2[, (n_label + 1)] <- as.matrix(h2o.predict(model2, test_hex))
}

# shutdown
h2o.shutdown()

# compare overlap
for (n in 1:5) {
  tmp <- cbind(raw_sub1, raw_sub2)
  plot(tmp[, n+1], tmp[, n+7])
}
