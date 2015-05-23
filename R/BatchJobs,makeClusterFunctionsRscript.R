#' @importFrom BatchJobs makeClusterFunctions makeSubmitJobResult
makeClusterFunctionsRscript <- function(parallel=FALSE) {
  submitJob = function(conf, reg, job.name, rscript, log.file, job.dir, resources, arrayjobs) {
    system2(command = file.path(R.home("bin"), "Rscript"),
            args = rscript,
            stdout = log.file,
            stderr = log.file,
            wait = !parallel)
    makeSubmitJobResult(status = 0L, batch.job.id = "cfLocal")
  }

  killJob = function(conf, reg, batch.job.id) NULL

  listJobs = function(conf, reg) {
    getJobParentDir <- get("getJobParentDir", mode="function", envir=getNamespace("BatchJobs"))
    fd = reg$file.dir
    jd = getJobParentDir(fd)
    jobdirs = dir(path=jd, pattern="^[0-9]+$", full.names=TRUE)
    ids <- as.integer(basename(jobdirs))
    ids
  }

  getArrayEnvirName = function() NA_character_

  name <- if (parallel) "multirscript" else "rscript"
  makeClusterFunctions(name = name, submitJob = submitJob, killJob = killJob,
                       listJobs = listJobs, getArrayEnvirName = getArrayEnvirName)
}
