## covr: skip=all
#' @importFrom R.utils removeDirectory
.onUnload <- function(libpath) {
  ## (a) Force finalize() on Future objects and their BatchJobs directories
  gc()
  ## (b) Remove BatchJobs root directory unless non-empty
  path <- asyncPath()
  try(removeDirectory(path, recursive=FALSE, mustExist=FALSE), silent=TRUE)
}

.onLoad <- function(libname, pkgname) {
  backend("default")
}

