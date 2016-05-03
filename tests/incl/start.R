library("future.BatchJobs")

## Record original state
ovars <- ls()
oopts <- options(warn=1, future.debug=TRUE)
oopts$future.delete <- getOption("future.delete")
oplan <- plan()
obackend <- backend()

## Use BatchJobs futures by default
plan(batchjobs)

## Important: If 'interactive' is used, the current evaluation
## environment is contaminated by the jobs, which will assign values,
## including the exported globals, to the current evaluation environment.
if (FALSE && Sys.getenv("_R_CHECK_FULL_") != "") {
  TEST_BACKENDS <- c("interactive", "local", "multicore=2")
  backend(c("multicore=2", "local"))
} else {
  TEST_BACKENDS <- c("interactive", "local")
  backend("local")
}

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
