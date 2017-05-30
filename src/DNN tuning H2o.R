## http://learn.h2o.ai/content/tutorials/deeplearning/

## R installation instructions are at http://h2o.ai/download
library(h2o)
h2o.init(nthreads=-1, max_mem_size="2G")
h2o.removeAll() ## clean slate - just in case the cluster was already running

args(h2o.deeplearning)
help(h2o.deeplearning)
example(h2o.deeplearning)
#demo(h2o.deeplearning)  #requires user interaction

setwd("C:/Users/smithlou/Downloads") ##For RStudio
spiral <- h2o.importFile(path = normalizePath("../data/spiral.csv"))
grid   <- h2o.importFile(path = normalizePath("../data/grid.csv"))
# Define helper to plot contours
plotC <- function(name, model, data=spiral, g=grid) {
  data <- as.data.frame(data) #get data from into R
  pred <- as.data.frame(h2o.predict(model, g))
  n=0.5*(sqrt(nrow(g))-1); d <- 1.5; h <- d*(-n:n)/n
  plot(data[,-3],pch=19,col=data[,3],cex=0.5,
       xlim=c(-d,d),ylim=c(-d,d),main=name)
  contour(h,h,z=array(ifelse(pred[,1]=="Red",0,1),
                      dim=c(2*n+1,2*n+1)),col="blue",lwd=2,add=T)
}

#dev.new(noRStudioGD=FALSE) #direct plotting output to a new window
par(mfrow=c(2,2)) #set up the canvas for 2x2 plots
plotC( "DL", h2o.deeplearning(1:2,3,spiral,epochs=1e3))
plotC("GBM", h2o.gbm         (1:2,3,spiral))
plotC("DRF", h2o.randomForest(1:2,3,spiral))
plotC("GLM", h2o.glm         (1:2,3,spiral,family="binomial"))

#dev.new(noRStudioGD=FALSE) #direct plotting output to a new window
par(mfrow=c(2,2)) #set up the canvas for 2x2 plots
ep <- c(1,250,500,750)
plotC(paste0("DL ",ep[1]," epochs"),
      h2o.deeplearning(1:2,3,spiral,epochs=ep[1],
                       model_id="dl_1"))
plotC(paste0("DL ",ep[2]," epochs"),
      h2o.deeplearning(1:2,3,spiral,epochs=ep[2],
                       checkpoint="dl_1",model_id="dl_2"))
plotC(paste0("DL ",ep[3]," epochs"),
      h2o.deeplearning(1:2,3,spiral,epochs=ep[3],
                       checkpoint="dl_2",model_id="dl_3"))
plotC(paste0("DL ",ep[4]," epochs"),
      h2o.deeplearning(1:2,3,spiral,epochs=ep[4],
                       checkpoint="dl_3",model_id="dl_4"))

#dev.new(noRStudioGD=FALSE) #direct plotting output to a new window
par(mfrow=c(2,2)) #set up the canvas for 2x2 plots
for (hidden in list(c(11,13,17,19),c(42,42,42),c(200,200),c(1000))) {
  plotC(paste0("DL hidden=",paste0(hidden, collapse="x")),
        h2o.deeplearning(1:2,3,spiral,hidden=hidden,epochs=500))
}

  #dev.new(noRStudioGD=FALSE) #direct plotting output to a new window
  par(mfrow=c(2,2)) #set up the canvas for 2x2 plots
  
for (act in c("Tanh","Maxout","Rectifier","RectifierWithDropout")) {
  plotC(paste0("DL ",act," activation"), 
        h2o.deeplearning(1:2,3,spiral,
                         activation=act,hidden=c(100,100),epochs=1000))
}


  

  
setwd("C:/Users/smithlou/Downloads")
dt <- data.table::fread("covtype.csv")
dt[, V55 := as.factor(V55)]
df <- as.h2o(dt)
rm(dt)
# df <- h2o.importFile(path = normalizePath("covtype.csv"))
  dim(df)
  df
  splits <- h2o.splitFrame(df, c(0.6,0.2), seed=1234)
  train  <- h2o.assign(splits[[1]], "train.hex") # 60%
  valid  <- h2o.assign(splits[[2]], "valid.hex") # 20%
  test   <- h2o.assign(splits[[3]], "test.hex")  # 20%
  
  

  #dev.new(noRStudioGD=FALSE) #direct plotting output to a new window
  par(mfrow=c(1,1)) # reset canvas
  plot(h2o.tabulate(df, "Elevation",                       "Cover_Type"))
  plot(h2o.tabulate(df, "Horizontal_Distance_To_Roadways", "Cover_Type"))
  plot(h2o.tabulate(df, "Soil_Type",                       "Cover_Type"))
  plot(h2o.tabulate(df, "Horizontal_Distance_To_Roadways", "Elevation" ))
  
  
  response <- "V55"
  predictors <- setdiff(names(df), response)
  predictors
  
  
  m1 <- h2o.deeplearning(
    model_id="dl_model_first", 
    training_frame=train, 
    validation_frame=valid,   ## validation dataset: used for scoring and early stopping
    x=predictors,
    y=response,
    activation="Rectifier",  ## default
    hidden=c(200,200),       ## default: 2 hidden layers with 200 neurons each
    epochs=1,
    variable_importances=T    ## not enabled by default
  )
  summary(m1)
  
  
  head(as.data.frame(h2o.varimp(m1)))
  
  
  # stop early if not improving
  m2 <- h2o.deeplearning(
    model_id="dl_model_faster", 
    training_frame=train, 
    validation_frame=valid,
    x=predictors,
    y=response,
    hidden=c(32,32,32),                  ## small network, runs faster
    epochs=1000000,                      ## hopefully converges earlier...
    score_validation_samples=10000,      ## sample the validation dataset (faster)
    stopping_rounds=2,
    stopping_metric="misclassification", ## could be "MSE","logloss","r2"
    stopping_tolerance=0.01
  )
  summary(m2)
  plot(m2)
  
  
  m3 <- h2o.deeplearning(
    model_id="dl_model_tuned", 
    training_frame=train, 
    validation_frame=valid, 
    x=predictors, 
    y=response, 
    overwrite_with_best_model=F,    ## Return the final model after 10 epochs, even if not the best
    hidden=c(128,128,128),          ## more hidden layers -> more complex interactions
    epochs=10,                      ## to keep it short enough
    score_validation_samples=10000, ## downsample validation set for faster scoring
    score_duty_cycle=0.025,         ## don't score more than 2.5% of the wall time
    adaptive_rate=F,                ## manually tuned learning rate
    rate=0.01, 
    rate_annealing=2e-6,            
    momentum_start=0.2,             ## manually tuned momentum
    momentum_stable=0.4, 
    momentum_ramp=1e7, 
    l1=1e-5,                        ## add some L1/L2 regularization
    l2=1e-5,
    max_w2=10                       ## helps stability for Rectifier
  ) 
  summary(m3)
  
  
  h2o.performance(m3, train=T)          ## sampled training data (from model building)
  h2o.performance(m3, valid=T)          ## sampled validation data (from model building)
  h2o.performance(m3, newdata=train)    ## full training data
  h2o.performance(m3, newdata=valid)    ## full validation data
  h2o.performance(m3, newdata=test)     ## full test data
  
  
  pred <- h2o.predict(m3, test)
  pred
  
  # had to define test here??
  test$Accuracy <- pred$predict == test$V55
  1-mean(test$Accuracy)
  
  
  sampled_train=train[1:10000,]
  
    hyper_params <- list(
      hidden=list(c(32,32,32),c(64,64)),
      input_dropout_ratio=c(0,0.05),
      rate=c(0.01,0.02),
      rate_annealing=c(1e-8,1e-7,1e-6)
    )
  
    grid <- h2o.grid(
    algorithm="deeplearning",
    grid_id="dl_grid", 
    training_frame=sampled_train,
    validation_frame=valid, 
    x=predictors, 
    y=response,
    epochs=10,
    stopping_metric="misclassification",
    stopping_tolerance=1e-2,        ## stop when misclassification does not improve by >=1% for 2 scoring events
    stopping_rounds=2,
    score_validation_samples=10000, ## downsample validation set for faster scoring
    score_duty_cycle=0.025,         ## don't score more than 2.5% of the wall time
    adaptive_rate=F,                ## manually tuned learning rate
    momentum_start=0.5,             ## manually tuned momentum
    momentum_stable=0.9, 
    momentum_ramp=1e7, 
    l1=1e-5,
    l2=1e-5,
    activation=c("Rectifier"),
    max_w2=10,                      ## can help improve stability for Rectifier
    hyper_params=hyper_params
  )
  grid
  
  grid <- h2o.getGrid("dl_grid",sort_by="err",decreasing=FALSE)
  grid
  
  ## To see what other "sort_by" criteria are allowed
  #grid <- h2o.getGrid("dl_grid",sort_by="wrong_thing",decreasing=FALSE)
  
  ## Sort by logloss
  h2o.getGrid("dl_grid",sort_by="logloss",decreasing=FALSE)
  
  ## Find the best model and its full set of parameters
  grid@summary_table[1,]
  best_model <- h2o.getModel(grid@model_ids[[1]])
  best_model
  
  print(best_model@allparameters)
  print(h2o.performance(best_model, valid=T))
  print(h2o.logloss(best_model, valid=T))
  
  
  hyper_params <- list(
    activation=c("Rectifier","Tanh","Maxout","RectifierWithDropout","TanhWithDropout","MaxoutWithDropout"),
    hidden=list(c(20,20),c(50,50),c(30,30,30),c(25,25,25,25)),
    input_dropout_ratio=c(0,0.05),
    l1=seq(0,1e-4,1e-6),
    l2=seq(0,1e-4,1e-6)
  )
  hyper_params
  
  ## Stop once the top 5 models are within 1% of each other (i.e., the windowed average varies less than 1%)
  search_criteria = list(strategy = "RandomDiscrete", max_runtime_secs = 360, max_models = 100, seed=1234567, stopping_rounds=5, stopping_tolerance=1e-2)
  dl_random_grid <- h2o.grid(
    algorithm="deeplearning",
    grid_id = "dl_grid_random",
    training_frame=sampled_train,
    validation_frame=valid, 
    x=predictors, 
    y=response,
    epochs=1,
    stopping_metric="logloss",
    stopping_tolerance=1e-2,        ## stop when logloss does not improve by >=1% for 2 scoring events
    stopping_rounds=2,
    score_validation_samples=10000, ## downsample validation set for faster scoring
    score_duty_cycle=0.025,         ## don't score more than 2.5% of the wall time
    max_w2=10,                      ## can help improve stability for Rectifier
    hyper_params = hyper_params,
    search_criteria = search_criteria
  )                                
  grid <- h2o.getGrid("dl_grid_random",sort_by="logloss",decreasing=FALSE)
  grid
  
  grid@summary_table[1,]
  best_model <- h2o.getModel(grid@model_ids[[1]]) ## model with lowest logloss
  best_model
  
  
  
  grid <- h2o.getGrid("dl_grid",sort_by="err",decreasing=FALSE)
  best_model <- h2o.getModel(grid@model_ids[[1]]) ## model with lowest classification error (on validation, since it was available during training)
  h2o.confusionMatrix(best_model,valid=T)
  best_params <- best_model@allparameters
  best_params$activation
  best_params$hidden
  best_params$input_dropout_ratio
  best_params$l1
  best_params$l2
  
  
  max_epochs <- 12 ## Add two more epochs
  m_cont <- h2o.deeplearning(
    model_id="dl_model_tuned_continued", 
    checkpoint="dl_model_tuned", 
    training_frame=train, 
    validation_frame=valid, 
    x=predictors, 
    y=response, 
    hidden=c(128,128,128),          ## more hidden layers -> more complex interactions
    epochs=max_epochs,              ## hopefully long enough to converge (otherwise restart again)
    stopping_metric="logloss",      ## logloss is directly optimized by Deep Learning
    stopping_tolerance=1e-2,        ## stop when validation logloss does not improve by >=1% for 2 scoring events
    stopping_rounds=2,
    score_validation_samples=10000, ## downsample validation set for faster scoring
    score_duty_cycle=0.025,         ## don't score more than 2.5% of the wall time
    adaptive_rate=F,                ## manually tuned learning rate
    rate=0.01, 
    rate_annealing=2e-6,            
    momentum_start=0.2,             ## manually tuned momentum
    momentum_stable=0.4, 
    momentum_ramp=1e7, 
    l1=1e-5,                        ## add some L1/L2 regularization
    l2=1e-5,
    max_w2=10                       ## helps stability for Rectifier
  ) 
  summary(m_cont)
  plot(m_cont)
  
  
  path <- h2o.saveModel(m_cont, 
                      path="./mybest_deeplearning_covtype_model", force=TRUE)
  print(path)
  m_loaded <- h2o.loadModel(path)
  summary(m_loaded)
  
 
  dlmodel <- h2o.deeplearning(
    x=predictors,
    y=response, 
    training_frame=train,
    hidden=c(10,10),
    epochs=1,
    nfolds=5,
    fold_assignment="Modulo" # can be "AUTO", "Modulo", "Random" or "Stratified"
  )
  dlmodel
  
  
  train$bin_response <- ifelse(train[,response]=="class_1", 0, 1)
  
dlmodel <- h2o.deeplearning(
  x=predictors,
  y="bin_response", 
  training_frame=train,
  hidden=c(10,10),
  epochs=0.1
)
summary(dlmodel)


train$bin_response <- as.factor(train$bin_response) ##make categorical

response2 <- "bin_response"
dlmodel <- h2o.deeplearning(
  x=predictors,
  y="bin_response",
  training_frame=train,
  hidden=c(10,10),
  epochs=0.1
  #balance_classes=T    ## enable this for high class imbalance
)
summary(dlmodel) ## Now the model metrics contain AUC for binary classification
plot(h2o.performance(dlmodel)) ## display ROC curve


  
   
  