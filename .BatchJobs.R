cluster.functions <- local({
  hows <- c("interactive", "local", "rscript", "multirscript", "multicore")
  how <- if (.Platform$OS == "windows") "local" else "multicore"
  if (how == "interactive") {
    makeClusterFunctionsInteractive()
  } else if (how == "local") {
    makeClusterFunctionsLocal()
  } else if (how == "rscript") {
    async:::makeClusterFunctionsRscript(parallel=FALSE)
  } else if (how == "multirscript") {
    async:::makeClusterFunctionsRscript(parallel=TRUE)
  } else if (how == "multicore") {
    ncpus <- parallel::detectCores()
    makeClusterFunctionsMulticore(ncpus=ncpus)
  }
})
