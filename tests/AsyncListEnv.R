library("R.utils")
library("async")

ovars <- ls(envir=globalenv())
oopts <- options(future=async, warn=1, "async::debug"=TRUE)

backend("local")

message("*** AsyncListEnv: Allocation (empty)")
x <- AsyncListEnv()
print(x)
stopifnot(is.na(inspect(x[[1]])))

message("*** AsyncListEnv: Assignment by name")
x$a <- 1
stopifnot(is.na(inspect(x$a)))
stopifnot(is.na(inspect(x[[1]])))

message("*** AsyncListEnv: Assignment by index")
x[[1]] <- 1.1
stopifnot(is.na(inspect(x$a)))
stopifnot(is.na(inspect(x[[1]])))

x[[2]] <- 2
stopifnot(is.na(inspect(x[[2]])))


message("*** AsyncListEnv: Allocation (length 3)")
x <- AsyncListEnv(length=3L)
names(x) <- c("a", "b", "c")
print(x)

message("*** AsyncListEnv: Asynchroneous evaluation (by name)")
x$a %<=% { 1 }
x$b %<=% { stop("Wow!"); 2 }
x$c %<=% { list(foo=3, bar=letters) }
print(x)

message("*** AsyncListEnv: Inspection")
tasks <- inspect(envir=x)
print(tasks)

message("*** AsyncListEnv: Asynchroneous evaluation (by index)")
x[[1]] %<=% { 1 }
x[[2]] %<=% { stop("Wow!"); 2 }
x[[3]] %<=% { list(foo=3, bar=letters) }
print(x)


message("*** AsyncListEnv: Inspection")
tasks <- inspect(envir=x)
print(tasks)


message("*** AsyncListEnv: Wait for asynchroneous evaluation to complete")
## Wait for all jobs to finish
count <- 0L
while (!all(finished(x)) && count < 10L) {
  cat(".")
  Sys.sleep(1)
  count <- count + 1L
}
cat("\n")
stopifnot(all(finished(x)))
if (any(failed(x))) print(error(x))


message("*** AsyncListEnv: Reassign asynchroneous evaluation")
x[[2]] %<=% { 2 }

print(x)
print(status(x))
print(finished(x))
print(completed(x))
print(failed(x))
print(expired(x))

## Wait for all jobs to finish
count <- 0L
while (!all(finished(x)) && count < 10L) {
  cat(".")
  Sys.sleep(1)
  count <- count + 1L
}
cat("\n")
stopifnot(all(finished(x)))
if (any(failed(x))) print(error(x))
print(value(x))


## Cleanup
options(oopts)
rm(list=setdiff(ls(envir=globalenv()), ovars), envir=globalenv())
