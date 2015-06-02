library("async")

ovars <- ls(envir=globalenv())
oopts <- options(warn=1, "async::debug"=TRUE)
obe <- backend(c("multicore=2", "local"))

task <- BatchJobsAsyncTask({ x <- 1 })
print(task)

try(print(delete(task)))
try(print(delete(task)))


## Cleanup
options(oopts)
rm(list=setdiff(ls(envir=globalenv()), ovars), envir=globalenv())
