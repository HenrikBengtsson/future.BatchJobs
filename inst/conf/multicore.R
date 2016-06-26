cluster.functions <- local({
  workers <- future::availableCores(constraints="multicore")
  if (workers == 1L) {
    makeClusterFunctionsLocal()
  } else {
    makeClusterFunctionsMulticore(ncpus=workers)
  }
})
