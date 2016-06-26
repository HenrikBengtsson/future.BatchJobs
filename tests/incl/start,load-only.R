## Record original state
ovars <- ls()
oopts <- options(warn=1L, mc.cores=2L, future.debug=TRUE)
oopts$future.delete <- getOption("future.delete")
oplan <- future::plan()
obackend <- future.BatchJobs::backend()

## Use BatchJobs futures by default
future::plan(future.BatchJobs:::batchjobs_local)

fullTest <- (Sys.getenv("_R_CHECK_FULL_") != "")

## Important: If 'interactive' is used, the current evaluation
## environment is contaminated by the jobs, which will assign values,
## including the exported globals, to the current evaluation environment.
if (FALSE && Sys.getenv("_R_CHECK_FULL_") != "") {
  TEST_BACKENDS <- c("interactive", "local", "multicore=2")
  future.BatchJobs::backend(c("multicore=2", "local"))
} else {
  TEST_BACKENDS <- c("interactive", "local")
  future.BatchJobs::backend("local")
}

await <- future.BatchJobs:::await
delete <- future.BatchJobs:::delete
attachedPackages <- future.BatchJobs:::attachedPackages
tempRegistry <- future.BatchJobs:::tempRegistry
isNA <- future.BatchJobs:::isNA
isFALSE <- future.BatchJobs:::isFALSE
trim <- future.BatchJobs:::trim
hpaste <- future.BatchJobs:::hpaste
printf <- future.BatchJobs:::printf
mcat <- future.BatchJobs:::mcat
mprintf <- future.BatchJobs:::mprintf
mprint <- future.BatchJobs:::mprint
mstr <- future.BatchJobs:::mstr
importFuture <- future.BatchJobs:::importFuture
importBatchJobs <- future.BatchJobs:::importBatchJobs
