isNA <- function(x) {
  if (length(x) != 1L) return(FALSE)
  is.na(x)
}

isFALSE <- function(x) {
  if (length(x) != 1L) return(FALSE)
  x <- as.logical(x)
  x <- unclass(x)
  identical(FALSE, x)
}

attachedPackages <- function() {
  pkgs <- search()
  pkgs <- grep("^package:", pkgs, value=TRUE)
  pkgs <- gsub("^package:", "", pkgs)
  pkgs
}
