R.utils::use("async")

ovars <- ls(envir=globalenv())
oopts <- options(warn=1, "async::debug"=FALSE)

expr <- BatchJobsAsyncTask({ x <- 1 })
print(expr)

## Cleanup
options(oopts)
rm(list=setdiff(ls(envir=globalenv()), ovars), envir=globalenv())
