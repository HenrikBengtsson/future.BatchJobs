.onLoad <- function(libname, pkgname) {
  inRCmdCheck <- import_future("inRCmdCheck")
  if (inRCmdCheck()) {
    path <- Sys.getenv("R_FUTURE_CACHE_PATH", file.path(tempdir(), ".future"))
    Sys.setenv("R_FUTURE_CACHE_PATH" = path)
  }
}

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


.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Package 'future.BatchJobs' is deprecated and will be archived on CRAN. Please use 'future.batchtools' instead.")
}
