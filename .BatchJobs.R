cluster.functions <- local({
  backend <- if (.Platform$OS == "windows") "local" else "multicore"
  backend <- Sys.getenv("R_ASYNC_BACKEND", backend)
##  message(".BatchJobs.R: Package 'async' using backend '", backend, "'")
##  backend <- "tipcc"
##  backend <- "cccore070"
  if (backend == "interactive") {
    makeClusterFunctionsInteractive()
  } else if (backend == "local") {
    makeClusterFunctionsLocal()
  } else if (backend == "rscript") {
    async:::makeClusterFunctionsRscript(parallel=FALSE)
  } else if (backend == "multirscript") {
    async:::makeClusterFunctionsRscript(parallel=TRUE)
  } else if (backend == "multicore") {
    ncpus <- parallel::detectCores()
    makeClusterFunctionsMulticore(ncpus=ncpus)
  } else if (backend == "torque") {
    paths <- c(".", "~", system.file(package="async", "config"))
    tmpl <- file.path(paths, "pbs.tmpl")
    tmpl <- tmpl[file_test("-f", tmpl)]
    stopifnot(length(tmpl) > 0L)
    tmpl <- tmpl[1L]
    message(".BatchJobs.R: Package 'async' using torque template file'", tmpl, "'")
    makeClusterFunctionsTorque(tmpl)
  } else if (backend == "ssh") {
    makeClusterFunctionsSSH(
      makeSSHWorker(nodename="n6", max.jobs=2),
      makeSSHWorker(nodename="n8"),
      makeSSHWorker(nodename="n12")
    )
  } else {
    stop("Unknown backend: ", backend)
  }
})

