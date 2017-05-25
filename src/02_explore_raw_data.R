# plot raw data for single epoch
p1 <- eval1.data[datetime >= ymd_hms("2016-06-14 06:00:00") & datetime <= ymd_hms("2016-06-14 07:00:00")] %>%
  ggplot(aes(datetime, vec.mag, group = device_id)) + 
  geom_line(aes(col = as.factor(steps))) +
  facet_wrap(~device_id, ncol = 1) + 
  labs(title = "Example of raw data for a single 5 second epoch starting at 2016-05-04 18:45:00",
       subtitle = "The lines are coloured by the number of steps identified by the Actilife software in the epoch.",
       caption = "NOTE: devices times are not 100% synchronous +/- 1s",
       y = "Accelerometer Vector Magnitude",
       x = "Time (s)") + 
  theme_bw() +
  scale_colour_discrete(name = "Steps in epoch") + 
  theme(legend.position = "bottom",
        legend.key = element_rect(colour = "white"),
        panel.grid.minor = element_blank())
p1
pdf("graphs/sampleRawPattern1.pdf",compress = FALSE )
p1
dev.off()


# plot raw data for single epoch

train.data[device_id == "TAS1E31150003", subj_type = "human"]

p2 <- train.data[,subj_type := ifelse(device_id %in% c("TAS1E31150003", "TAS1E31150028"), "human", "dog") , ][
  ,title := paste(device_id, "-", subj_type ),][datetime >= ymd_hms("2016-05-04 18:33:30") & datetime < ymd_hms("2016-05-04 18:33:35")& device_id != "TAS1E31150005"] %>%
  ggplot(aes(datetime, vec.mag, group = device_id)) + 
  geom_line() +
  facet_wrap(~title, ncol = 1) + 
  labs(title = "Example of raw data for a single 5 second epoch starting at 18:33:30",
       subtitle = "The algorithm found an equal number of steps for each device.
However it is apparent that there are significant differences in the raw data profiles.",
       caption = "NOTE: devices times are not 100% synchronous +/- 1s",
       y = "Accelerometer Vector Magnitude",
       x = "Time (s)") + 
  theme_bw() +
  scale_colour_discrete(name = "Steps in epoch") + 
  theme(legend.position = "bottom",
        legend.key = element_rect(colour = "white"),
        panel.grid.minor = element_blank())
p2
pdf("graphs/sampleRawPattern2.pdf",compress = FALSE )
p2
dev.off()

data[epoch_id == ymd_hms("2016-05-04 18:33:30"), sd(steps), device_id]
