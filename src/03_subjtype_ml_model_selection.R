# create model.data, filtering out epochs with low steps
## drop steps - only want numeric
model.data <- poc.summary

# run models to select best performing model type on training data (/louis1)
set.seed(98315)
# set how many runs to do
n_resamples <- 10
seeds <- round(rep(runif(n_resamples, 1, 10000000)), 0)

# generate models using 30 different seeds to split training / testing and store Kappa values
## C5.0 TAKES A LONG TIME TO CONVERGE
subjtype.resample.results <- do.call(rbind, 
                              lapply(seeds, function(s) {
                                print(which(seeds ==s))
                                return(subjtype.check.model.perf(s, 
                                                                 model.data, 
                                                                 models = c('nnet', 'gbm', 'rf')))
                              })
                            )

# summary of kappa values for each model method for the 30 runs
summary(subjtype.resample.results)
# cache
cache("subjtype.resample.results")

# plot resultant Kappa values
p4 <- ggplot(melt(subjtype.resample.results, value.name = "kappa") %>% 
         mutate(Var2 = str_replace(Var2, ".Kappa", "")),
       aes(x=Var2, y=kappa)) + 
  geom_violin() +
  geom_boxplot(fill = "grey", alpha = 0.25) + 
  geom_point(alpha = 0.25) +
  theme_bw() +
  labs(title = "Kappa Results from Models - 30 Data Partitions",
       subtitle = "The evaluation results of models show some variation in performance depending on the data split.
In general models are showing strong predictive power.",
       caption = "Grey box represents IQR with Median\nViolin plot shows distribution",
       x="",
       y="") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        panel.grid.major.x = element_blank()) 

p4
pdf("graphs/subjtypeModelResampleKappa.pdf", compress = FALSE)
p4
dev.off()
