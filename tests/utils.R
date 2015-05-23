library("async")

isNA <- async:::isNA
isFALSE <- async:::isFALSE

stopifnot(isNA(NA), !isNA(TRUE), !isNA(FALSE), !isNA(1),
          !isNA(NULL), !isNA(1:2), !isNA(rep(NA,3)),
          !isNA(rep(TRUE,3)), !isNA(letters))

stopifnot(isFALSE(FALSE), !isFALSE(TRUE), !isFALSE(NA), !isFALSE(1),
          !isFALSE(NULL), !isFALSE(1:2), !isFALSE(rep(FALSE,3)),
          !isFALSE(rep(TRUE,3)), !isFALSE(letters))

