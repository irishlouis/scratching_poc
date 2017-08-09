# look at some scratching events
p1 <- plot_raw_epoch(dt = poc.raw.data, epoch = "2017-05-23 12:23:40")
p2 <- plot_raw_epoch(dt = poc.raw.data, epoch = "2017-05-23 12:27:00")
p3 <- plot_raw_epoch(dt = poc.raw.data, epoch = "2017-05-23 18:16:00")
p4 <- plot_raw_epoch(dt = poc.raw.data, epoch = "2017-05-23 15:07:00")

p <- poc.raw.data %>% 
  filter(epoch_id >= ymd_hms("20170523 111500"), epoch_id < ymd_hms("20170523 113000")) %>%
  ggplot(aes(datetime, vec.mag)) + 
  geom_line() +
  theme_bw() + 
  geom_vline(xintercept = as.numeric(ymd_hms("2017-05-23 11:22:00")), col = "red") +
  geom_vline(xintercept = as.numeric(ymd_hms("2017-05-23 11:22:30")), col = "red") +
  labs(title = "15min raw accelerometer data for Subject A",
       subtitle = "Period of scratching marked in red",
       x = "",
       y = "Vector Magnitute (m/s^2)")
p
p1 <- poc.raw.data %>% 
  filter(epoch_id >= ymd_hms("20170523 112200"), epoch_id < ymd_hms("20170523 112230")) %>%
  ggplot(aes(datetime, vec.mag)) + 
  geom_line() +
  theme_bw() + 
  geom_vline(xintercept = as.numeric(ymd_hms("2017-05-23 11:22:00")), col = "red") +
  geom_vline(xintercept = as.numeric(ymd_hms("2017-05-23 11:22:30")), col = "red") +
  labs(title = "Raw scratching event accelerometer data for Subject A",
       subtitle = "Period of scratching marked in red",
       x = "",
       y = "Vector Magnitute (m/s^2)")
p1
p

grid.arrange(p, p1, ncol = 1)
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


