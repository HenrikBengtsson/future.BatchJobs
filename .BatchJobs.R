cluster.functions <- local({
  backend <- if (.Platform$OS == "windows") "local" else "multicore"
  backend <- Sys.getenv("R_ASYNC_BACKEND", backend)
##  backend <- "tipcc"
  backend <- "cccore070"
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
    makeClusterFunctionsTorque(tmpl)
  } else {
    stop("Unknown backend: ", backend)
  }
})

