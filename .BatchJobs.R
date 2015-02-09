cluster.functions <- local({
  how <- c("interactive", "local", "rscript", "multirscript", "multicore")[3]
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
