# prepare data for modelling
scale.cols <- c("avg.vec","sd.vec","peaks.per.sec",
                "avg.period","sd.period","avg.amp",
                "sd.amp")

poc.summary[, (scale.cols) := lapply(.SD, scale), , 
            .SDcols = scale.cols]


test2.summary[, (scale.cols) := lapply(.SD, scale), , .SDcols = scale.cols]

head(poc.summary)
cache("poc.summary")
rm(scale.cols)