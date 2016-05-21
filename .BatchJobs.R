cluster.functions <- local({
  backend <- if (.Platform$OS.type == "windows") "local" else "multicore"
  backend <- Sys.getenv("R_FUTURE_BATCHJOBS_BACKEND", backend)
  if (backend == "interactive") {
    makeClusterFunctionsInteractive()
  } else if (backend == "local") {
    makeClusterFunctionsLocal()
  } else if (backend == "multicore") {
    ncpus <- parallel::detectCores()
    makeClusterFunctionsMulticore(ncpus=ncpus)
  } else if (backend == "torque") {
    paths <- c(".", "~", system.file(package="future.BatchJobs", "conf"))
    tmpl <- file.path(paths, "torque.brew")
    tmpl <- tmpl[file_test("-f", tmpl)]
    stopifnot(length(tmpl) > 0L)
    tmpl <- tmpl[1L]
    message(".BatchJobs.R: Package 'future.BatchJobs' using torque template file'", tmpl, "'")
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

