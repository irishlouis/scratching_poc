###########################################################################
## clean slate
pkgs <- names(sessionInfo()$otherPkgs) 
if(!is.null(pkgs)){
  pkgs <- paste('package:', pkgs, sep = "")
  lapply(pkgs, detach, character.only = TRUE, unload = TRUE, force = TRUE)
}
rm(list = ls()[ls() != "run.munge"])
h2o::h2o.removeAll()
h2o::h2o.shutdown(prompt = FALSE)
###########################################################################

require(ProjectTemplate)
load.project()
rm(poc.raw.data)
rm(test1.raw.data)
rm(test2.raw.data)

###########################################################################

# load the h2o models
best_gbm <- h2o::h2o.loadModel("ml_models/Grid_GBM_RTMP_sid_aee7_5_model_R_1496865147756_1_model_2")
best_rf  <- h2o::h2o.loadModel("ml_models/Grid_DRF_RTMP_sid_aee7_5_model_R_1496865147756_27136_model_16")
best_dl  <- h2o::h2o.loadModel("ml_models/")
best_glm <- h2o::h2o.loadModel("ml_models/Grid_GLM_RTMP_sid_aee7_5_model_R_1496865147756_58078_model_7")   

###########################################################################

run.munge()

###########################################################################
publish.rmd()
# publish.rmd("pdf_document")  ## no pdf creator installed
publish.rmd("word_document")
###########################################################################