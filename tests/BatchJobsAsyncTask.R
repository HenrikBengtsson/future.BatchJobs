library("async")

ovars <- ls(envir=globalenv())
oopts <- options(warn=1, "async::debug"=FALSE)

task <- BatchJobsAsyncTask({ x <- 1 })
print(task)

## Cleanup
options(oopts)
rm(list=setdiff(ls(envir=globalenv()), ovars), envir=globalenv())
