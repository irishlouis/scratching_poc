# look at some scratching events
p1 <- plot_raw_epoch(dt = poc.raw.data, epoch = "2017-05-23 12:07:00")
p2 <- plot_raw_epoch(dt = poc.raw.data, epoch = "2017-05-23 12:27:00")
p3 <- plot_raw_epoch(dt = poc.raw.data, epoch = "2017-05-23 13:07:00")
p4 <- plot_raw_epoch(dt = poc.raw.data, epoch = "2017-05-23 15:07:00")

grid.arrange(p1, p2, p3, p4, ncol = 2)

# save plot as pdf to /graphs
# pdf("graphs/sampleRawPattern1.pdf",compress = FALSE )
# p1
# dev.off()

poc.summary %>% 
  melt(id.vars = c("device_id", "epoch_id", "scratch_event")) %>%
  ggplot(aes(scratch_event, value)) +
    geom_boxplot(aes(fill = scratch_event)) +
    theme_bw() +
    facet_wrap(~variable, scales = "free") +
    labs(x = "", y = "", title = "Distribution of metrics from summarised epoch data") +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          axis.text.x = element_blank())


