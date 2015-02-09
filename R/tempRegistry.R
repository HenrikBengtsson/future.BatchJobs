#' @importFrom R.utils tempvar
#' @importFrom BatchJobs makeRegistry
tempRegistry <- local({
  regs <- new.env()
  function(prefix="async", ...) {
    id <- tempvar(prefix=prefix, value=NA, envir=regs)
    makeRegistry(id=id, ...)
  }
})



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
    fd = reg$file.dir
    jd = BatchJobs:::getJobParentDir(fd)
    jobdirs = dir(path=jd, pattern="^[0-9]+$", full.names=TRUE)
    ids <- as.integer(basename(jobdirs))
    ids
  }

  getArrayEnvirName = function() NA_character_

  name <- if (parallel) "multirscript" else "rscript"
  makeClusterFunctions(name = name, submitJob = submitJob, killJob = killJob,
                       listJobs = listJobs, getArrayEnvirName = getArrayEnvirName)
}
