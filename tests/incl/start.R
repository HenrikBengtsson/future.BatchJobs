library("async")

## Record original state
ovars <- ls()
oopts <- options(warn=1, "async::debug"=TRUE)
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

attachedPackages <- async:::attachedPackages
tempRegistry <- async:::tempRegistry
isNA <- async:::isNA
isFALSE <- async:::isFALSE
trim <- async:::trim
hpaste <- async:::hpaste
printf <- async:::printf
mcat <- async:::mcat
mprintf <- async:::mprintf
mprint <- async:::mprint
mstr <- async:::mstr
