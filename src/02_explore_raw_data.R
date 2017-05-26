
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



