source("incl/start.R")

message("*** BatchJobsFuture() ...")

f <- BatchJobsFuture({ x <- 1 })
print(f)

try(print(delete(f)))
try(print(delete(f)))

message("*** BatchJobsFuture() - exceptions ...")

res <- try(f <- BatchJobsFuture(42L, workers=integer(0)), silent=TRUE)
print(res)
stopifnot(inherits(res, "try-error"))

res <- try(f <- BatchJobsFuture(42L, workers=0L), silent=TRUE)
print(res)
stopifnot(inherits(res, "try-error"))

res <- try(f <- BatchJobsFuture(42L, workers=TRUE), silent=TRUE)
print(res)
stopifnot(inherits(res, "try-error"))

message("*** BatchJobsFuture() - exceptions ... DONE")

message("*** BatchJobsFuture() ... DONE")

source("incl/end.R")
