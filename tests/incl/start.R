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

tempRegistry <- async:::tempRegistry
makeClusterFunctionsRscript <- async:::makeClusterFunctionsRscript
tweakExpression <- async:::tweakExpression
isNA <- async:::isNA
isFALSE <- async:::isFALSE


mcat <- function(...) message(..., appendLF=FALSE)

mprintf <- function(...) mcat(sprintf(...))

mprint <- function(..., appendLF=FALSE) {
  bfr <- capture.output(print(...))
  bfr <- paste(c(bfr, ""), collapse="\n")
  message(bfr, appendLF=appendLF)
}

mstr <- function(..., appendLF=FALSE) {
  bfr <- capture.output(str(...))
  bfr <- paste(c(bfr, ""), collapse="\n")
  message(bfr, appendLF=appendLF)
}
