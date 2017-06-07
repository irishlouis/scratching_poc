# test model on unseen subject data

nnet_unseen_predict <- data.frame(epoch_id =test2.summary$epoch_id,
                                  pred = predict(nnet_final, test2.summary))
table(nnet_unseen_predict$pred)

nnet_unseen_predict %>% filter(pred == "scratch")

