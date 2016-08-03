source("incl/start.R")

message("*** BatchJobsFuture() ...")

message("*** BatchJobsFuture() - cleanup ...")

f <- batchjobs_local({ 1L })
print(f)
res <- await(f, cleanup=TRUE)
print(res)
stopifnot(res == 1L)

message("*** BatchJobsFuture() - cleanup ... DONE")


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


message("*** BatchJobsFuture() - registry exceptions ...")

## Non-existing BatchJobs registry
f <- BatchJobsFuture({ x <- 1 })
print(f)

## Hack to emulate where BatchJobs registry is deleted or fails
f$state <- "running"
path <- f$config$reg$file.dir
unlink(path, recursive=TRUE)

res <- value(f, onMissing="default")
print(res)
stopifnot(is.null(res))

res <- tryCatch({
  value(f, onMissing="error")
}, error = function(ex) ex)
print(res)
stopifnot(inherits(res, "error"))

res <- tryCatch({
  await(f)
}, error = function(ex) ex)
print(res)
stopifnot(inherits(res, "error"))


message("*** BatchJobsFuture() - registry exceptions ... DONE")

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


message("*** BatchJobsFuture() - timeout ...")

if (fullTest && availableCores(constraints="multicore") > 1) {
  plan(batchjobs_multicore)
  
  options(future.wait.times=1L, future.wait.interval=0.1)
  
  f <- future({
    Sys.sleep(5)
    x <- 1
  })
  print(f)
  
  res <- tryCatch({
    value(f)
  }, error = function(ex) ex)
  stopifnot(inherits(res, "error"))
}


message("*** BatchJobsFuture() - timeout ... DONE")



message("*** BatchJobsFuture() ... DONE")

source("incl/end.R")
