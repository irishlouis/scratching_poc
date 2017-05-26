# Visualise the events from subject diary

# 10:11:20 10:11:40
poc.raw.data[epoch_id %in% sapply(-5:6, function(x) ymd_hms("2017-05-23 10:11:00") + seconds(x * 10) )] %>%
  ggplot(aes(datetime, vec.mag)) + geom_line() + facet_wrap(~epoch_id, ncol = 3, scales = "free_x")

# 10:27:10 10:27:20
poc.raw.data[epoch_id %in% sapply(-6:6, function(x) ymd_hms("2017-05-23 10:27:00") + seconds(x * 10) )] %>%
  ggplot(aes(datetime, vec.mag)) + geom_line() + facet_wrap(~epoch_id, ncol = 3, scales = "free_x")

# 11:22:00 11:22:20
poc.raw.data[epoch_id %in% sapply(-5:6, function(x) ymd_hms("2017-05-23 11:22:00") + seconds(x * 10) )] %>%
  ggplot(aes(datetime, vec.mag)) + geom_line() + facet_wrap(~epoch_id, ncol = 3, scales = "free_x")

# 11:55:20 11:55:40
poc.raw.data[epoch_id %in% sapply(-5:6, function(x) ymd_hms("2017-05-23 11:56:00") + seconds(x * 10) )] %>%
  ggplot(aes(datetime, vec.mag)) + geom_line() + facet_wrap(~epoch_id, ncol = 3, scales = "free_x")

# 12:02:20 12:03:00
poc.raw.data[epoch_id %in% sapply(-5:6, function(x) ymd_hms("2017-05-23 12:03:00") + seconds(x * 10) )] %>%
  ggplot(aes(datetime, vec.mag)) + geom_line() + facet_wrap(~epoch_id, ncol = 3, scales = "free_x")

# 12:23:30 12:25:50
poc.raw.data[epoch_id %in% sapply(-5:6, function(x) ymd_hms("2017-05-23 12:26:00") + seconds(x * 10) )] %>%
  ggplot(aes(datetime, vec.mag)) + geom_line() + facet_wrap(~epoch_id, ncol = 3, scales = "free_x")

# 12:37:30 12:38:10
poc.raw.data[epoch_id %in% sapply(-5:6, function(x) ymd_hms("2017-05-23 12:38:00") + seconds(x * 10) )] %>%
  ggplot(aes(datetime, vec.mag)) + geom_line() + facet_wrap(~epoch_id, ncol = 3, scales = "free_x")

# 12:51:20 12:51:40
poc.raw.data[epoch_id %in% sapply(-5:6, function(x) ymd_hms("2017-05-23 12:51:00") + seconds(x * 10) )] %>%
  ggplot(aes(datetime, vec.mag)) + geom_line() + facet_wrap(~epoch_id, ncol = 3, scales = "free_x")

# 14:57:00 14:57:20
poc.raw.data[epoch_id %in% sapply(-5:6, function(x) ymd_hms("2017-05-23 14:57:00") + seconds(x * 10) )] %>%
  ggplot(aes(datetime, vec.mag)) + geom_line() + facet_wrap(~epoch_id, ncol = 3, scales = "free_x")

# 16:45:50 16:46:00
poc.raw.data[epoch_id %in% sapply(-5:6, function(x) ymd_hms("2017-05-23 16:46:00") + seconds(x * 10) )] %>%
  ggplot(aes(datetime, vec.mag)) + geom_line() + facet_wrap(~epoch_id, ncol = 3, scales = "free_x")

# 17:44:50 17:45:10
poc.raw.data[epoch_id %in% sapply(-5:6, function(x) ymd_hms("2017-05-23 17:45:00") + seconds(x * 10) )] %>%
  ggplot(aes(datetime, vec.mag)) + geom_line() + facet_wrap(~epoch_id, ncol = 3, scales = "free_x")

# 18:15:50 18:16:10
poc.raw.data[epoch_id %in% sapply(-5:6, function(x) ymd_hms("2017-05-23 18:16:00") + seconds(x * 10) )] %>%
  ggplot(aes(datetime, vec.mag)) + geom_line() + facet_wrap(~epoch_id, ncol = 3, scales = "free_x")

# 19:15:50 19:16:00
poc.raw.data[epoch_id %in% sapply(-5:6, function(x) ymd_hms("2017-05-23 19:16:00") + seconds(x * 10) )] %>%
  ggplot(aes(datetime, vec.mag)) + geom_line() + facet_wrap(~epoch_id, ncol = 3, scales = "free_x")

# 19:16:50 19:17:10
poc.raw.data[epoch_id %in% sapply(-5:6, function(x) ymd_hms("2017-05-23 19:17:00") + seconds(x * 10) )] %>%
  ggplot(aes(datetime, vec.mag)) + geom_line() + facet_wrap(~epoch_id, ncol = 3, scales = "free_x")

# 19:33:20 19:33:30
poc.raw.data[epoch_id %in% sapply(-5:6, function(x) ymd_hms("2017-05-23 19:33:00") + seconds(x * 10) )] %>%
  ggplot(aes(datetime, vec.mag)) + geom_line() + facet_wrap(~epoch_id, ncol = 3, scales = "free_x")

# 19:54:50 19:55:10
poc.raw.data[epoch_id %in% sapply(-5:6, function(x) ymd_hms("2017-05-23 19:55:00") + seconds(x * 10) )] %>%
  ggplot(aes(datetime, vec.mag)) + geom_line() + facet_wrap(~epoch_id, ncol = 3, scales = "free_x")

# 20:48:20 20:48:40
poc.raw.data[epoch_id %in% sapply(-5:6, function(x) ymd_hms("2017-05-23 20:48:00") + seconds(x * 10) )] %>%
  ggplot(aes(datetime, vec.mag)) + geom_line() + facet_wrap(~epoch_id, ncol = 3, scales = "free_x")

# 07:52:10 07:52:30
poc.raw.data[epoch_id %in% sapply(-5:6, function(x) ymd_hms("2017-05-24 07:52:00") + seconds(x * 10) )] %>%
  ggplot(aes(datetime, vec.mag)) + geom_line() + facet_wrap(~epoch_id, ncol = 3, scales = "free_x")

# 08:54:20 08:54:40
poc.raw.data[epoch_id %in% sapply(-5:6, function(x) ymd_hms("2017-05-24 08:54:00") + seconds(x * 10) )] %>%
  ggplot(aes(datetime, vec.mag)) + geom_line() + facet_wrap(~epoch_id, ncol = 3, scales = "free_x")

# 09:08:40 09:09:00
poc.raw.data[epoch_id %in% sapply(-5:6, function(x) ymd_hms("2017-05-24 09:09:00") + seconds(x * 10) )] %>%
  ggplot(aes(datetime, vec.mag)) + geom_line() + facet_wrap(~epoch_id, ncol = 3, scales = "free_x")

# 09:19:10 09:19:30
poc.raw.data[epoch_id %in% sapply(-5:6, function(x) ymd_hms("2017-05-24 09:19:00") + seconds(x * 10) )] %>%
  ggplot(aes(datetime, vec.mag)) + geom_line() + facet_wrap(~epoch_id, ncol = 3, scales = "free_x")
