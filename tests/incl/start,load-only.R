## Record original state
ovars <- ls()
oopts <- options(
  warn = 1L,
  mc.cores = 2L,
  future.debug = FALSE,
  ## Speed up await() and delete():
  future.wait.interval = 0.05,
  future.wait.alpha = 1.001
)
oopts$future.delete <- getOption("future.delete")
oplan <- future::plan()

## Use local BatchJobs futures by default
future::plan(future.BatchJobs:::batchjobs_local)

Sys.setenv("R_FUTURE_CACHE_PATH" = file.path(tempdir(), ".future"))

fullTest <- (Sys.getenv("_R_CHECK_FULL_") != "")

isWin32 <- (.Platform$OS.type == "windows" && .Platform$r_arch == "i386")

attachedPackages <- future.BatchJobs:::attachedPackages
await <- future.BatchJobs:::await
delete <- future.BatchJobs:::delete
importBatchJobs <- future.BatchJobs:::importBatchJobs
importFuture <- future.BatchJobs:::importFuture
isNA <- future.BatchJobs:::isNA
isOS <- future.BatchJobs:::isOS
hpaste <- future.BatchJobs:::hpaste
mcat <- function(...) message(..., appendLF=FALSE)
mprintf <- function(...) message(sprintf(...), appendLF=FALSE)
mprint <- future.BatchJobs:::mprint
mstr <- future.BatchJobs:::mstr
printf <- future.BatchJobs:::printf
tempRegistry <- future.BatchJobs:::tempRegistry
trim <- future.BatchJobs:::trim
attachLocally <- function(x, envir=parent.frame()) {
  for (name in names(x)) {
    assign(name, value=x[[name]], envir=envir)
  }
}
