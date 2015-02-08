if (.Platform$OS != "windows") {
  cluster.functions = makeClusterFunctionsMulticore(
    ncpus=parallel::detectCores()
  );
} else {
  cluster.functions = makeClusterFunctionsLocal()
#  max.concurrent.jobs = parallel::detectCores()
}


###########################################################################
# HISTORY:
# 2013-08-26
# o Added makeClusterFunctionsRscript().
# o Created.
###########################################################################
