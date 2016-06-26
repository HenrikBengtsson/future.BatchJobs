## Restore original state
options(oopts)
future.BatchJobs::backend(obackend)
future::plan(oplan)
rm(list=c(setdiff(ls(), ovars)))
