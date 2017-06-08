# select columns to be scaled
scale.cols <- names(poc.summary)[sapply(poc.summary, is.numeric)]

# z scale each column for each of the summary data sets
zscale.poc.summary <- copy(poc.summary)[,(scale.cols) := lapply(.SD, scale), , .SDcols = scale.cols]
cache("zscale.poc.summary")

zscale.test1.summary <- copy(test1.summary)[,(scale.cols) := lapply(.SD, scale), , .SDcols = scale.cols]
cache("zscale.test1.summary")

zscale.test2.summary <- copy(test2.summary)[,(scale.cols) := lapply(.SD, scale), , .SDcols = scale.cols]
cache("zscale.test2.summary")

# housekeeping
rm(scale.cols)
