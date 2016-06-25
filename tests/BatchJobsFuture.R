source("incl/start.R")

message("*** BatchJobsFuture() ...")

message("*** BatchJobsFuture() - deleting exceptions ...")

## Deleting a non-resolved future
f <- BatchJobsFuture({ x <- 1 })
print(f)
res <- tryCatch({
  delete(f)
}, warning = function(w) w)
print(res)
stopifnot(inherits(res, "warning"))

## Printing a deleted future
f <- batchjobs(42L)
print(f)
v <- value(f)
print(v)
stopifnot(v == 42L)
res <- delete(f)
print(f)
res <- delete(f)
print(f)

message("*** BatchJobsFuture() - deleting exceptions ... DONE")


message("*** BatchJobsFuture() - value exceptions ...")

## Non-existing BatchJobs registry
f <- BatchJobsFuture({ x <- 1 })
print(f)

## Hack to emulate where BatchJobs registry is deleted or fails
f$config$reg <- NULL

res <- value(f, onMissing="default")
print(res)
stopifnot(is.null(res))

res <- tryCatch({
  value(f, onMissing="error")
}, error = function(ex) ex)
print(res)
stopifnot(inherits(res, "error"))

message("*** BatchJobsFuture() - value exceptions ... DONE")


message("*** BatchJobsFuture() - exceptions ...")

f <- BatchJobsFuture({ 42L })
print(f)
res <- tryCatch({
  loggedError(f)
}, error = function(ex) ex)
print(res)
stopifnot(inherits(res, "error"))

f <- BatchJobsFuture({ 42L })
print(f)
res <- tryCatch({
  loggedOutput(f)
}, error = function(ex) ex)
print(res)
stopifnot(inherits(res, "error"))

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
