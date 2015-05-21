library("R.utils")
library("async")

ovars <- ls(envir=globalenv())
oopts <- options(warn=1, "async::debug"=TRUE)

backend("local")

x <- AsyncListEnv(length=3L)
names(x) <- c("a", "b", "c")
print(x)

x[[1]] %<=% { 1 }
x[[2]] %<=% { stop("Wow!"); 2 }
x[[3]] %<=% { list(foo=3, bar=letters) }
print(x)

tasks <- inspectAll(x)
print(tasks)

## Wait for all jobs to finish
while (!all(finished(x))) { Sys.sleep(0.5) }
if (any(failed(x))) print(error(x))


x[[2]] %<=% { 2 }

print(x)
print(status(x))
print(finished(x))
print(failed(x))
print(expired(x))
print(value(x))

## Wait for all jobs to finish
while (!all(finished(x))) { Sys.sleep(0.5) }
if (any(failed(x))) print(error(x))


## Cleanup
options(oopts)
rm(list=setdiff(ls(envir=globalenv()), ovars), envir=globalenv())
