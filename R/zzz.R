## covr: skip=all
#' @importFrom R.utils removeDirectory isFile
.onUnload <- function(libpath) {
  ## (a) Force finalizer of Future objects to run such
  ##     that their BatchJobs directories are removed
  gc()

  ## (b) Remove BatchJobs root directory if only a set
  ##     of known files exists, i.e. not any directories etc.
  path <- futureCachePath(create=FALSE)
  ## Only known files left?
  files <- dir(path=path)
  knownFiles <- c("sessioninfo.txt")
  if (all(files %in% knownFiles)) {
    for (file in knownFiles) {
      pathnameT <- file.path(path, file)
      if (isFile(pathnameT)) try(file.remove(pathnameT))
    }
    try(removeDirectory(path, recursive=FALSE, mustExist=FALSE), silent=TRUE)
  }
}

#' @importFrom utils packageVersion
.onLoad <- function(libpath, pkgname) {
  ## NOTIFICATION: BatchJobs (> 1.6) and globals (<= 0.7.1)
  if ((verB <- packageVersion("BatchJobs")) > "1.6" &&
      packageVersion("globals") < "0.7.2") {
    warning(sprintf("future.BatchJobs requires globals (>= 0.7.2) when using BatchJobs %s.  Please update the globals package.", verB))
  }
}

