cluster.functions <- local({
  ## Replace the below with host names available on the ad hoc
  ## cluster you have access to, e.g. c("n6", "n8", "n12")
  nodes <- c("localhost", "localhost", "localhost")

  makeClusterFunctionsSSH(
    makeSSHWorker(nodename=nodes[1], max.jobs=2),
    makeSSHWorker(nodename=nodes[2]),
    makeSSHWorker(nodename=nodes[3])
  )
})
