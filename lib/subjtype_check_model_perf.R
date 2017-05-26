#' subjtype.check.model.perf
#' @description run list of models and return Kappa values, to run MC model validation
#'
#' @param seed - seed to split data on 
#' @param model.data - model data to partition
#'
#' @return Kappa values for models run
#' @export
#'
#' @examples
subjtype.check.model.perf <- function(seed, model.data, models, run.list = TRUE, tuneGrid = NULL){
  set.seed(seed)
  # partition data for building model
  s <- createDataPartition(model.data$scratch_event, p = 0.6, list = FALSE)
  training <- model.data[s][,':='(
    device_id = NULL,
    epoch_id = NULL)]
  testing <- model.data[-s][,':='(
    device_id = NULL,
    epoch_id = NULL)]
  
  # define model training control
  control1 <- caret::trainControl(
    method='repeatedcv',
    number=5,
    savePredictions=TRUE,
    classProbs=TRUE,
    search = "random"
  )
  control2 <- caret::trainControl(
    method='repeatedcv',
    number=5,
    savePredictions=TRUE,
    classProbs=TRUE
  )
  
  if(run.list){
  # train list of models using control spec'd above
    model_list <- caretList(scratch_event ~ ., 
                            data=training, 
                            trControl=control1,
                            methodList= models, 
                            tuneLength = 5)
  } else {
    model_list <- caretList(scratch_event ~ ., 
                            data=training, 
                            trControl=control2,
                            methodList= models, 
                            tuneGrid = tuneGrid)
    
  }
  
  # look at validation results for each 
  return(lapply(model_list, 
                function(x) confusionMatrix(predict(x, testing), testing$scratch_event)$overall[2]) %>% unlist)
}