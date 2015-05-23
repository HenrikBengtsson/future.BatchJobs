library("BatchJobs")
library("async")

ovars <- ls(envir=globalenv())
oopts <- options(warn=1, "async::debug"=TRUE)

tempRegistry <- async:::tempRegistry

## Important: If 'interactive' is used, the current evaluation
## environment is contaminated by the jobs, which will assign values,
## including the exported globals, to the current evaluation environment.
backend("local")


## Global string, which should be found instead of base::url().
url <- "http://www.r-project.org"

message("*** Function")
fun <- function(...) {
  cat("URL:\n")
  print(url)
  stopifnot(is.character(url), identical(url, "http://www.r-project.org"))
}
print(fun)

reg <- tempRegistry()
res <- batchExport(reg, li=list(url=url))
ids <- batchMap(reg, fun=fun, 1L)
res <- submitJobs(reg, ids=ids)
print(res)
showStatus(reg)


message("*** Expression")
expr <- substitute({
  cat("URL:\n")
  print(url)
  stopifnot(is.character(url), identical(url, "http://www.r-project.org"))
}, env=list())
print(expr)

reg <- tempRegistry()
res <- batchExport(reg, li=list(url=url))
ids <- batchEvalQ(reg, exprs=list(expr))
res <- submitJobs(reg, ids=ids)
print(res)

showStatus(reg)


message("*** Evaluation in a local({ ... }) call or not")
reg <- tempRegistry()
ids <- batchEvalQ(reg, exprs=list(expr), local=TRUE)
print(ids)
res <- submitJobs(reg, ids=ids)
print(res)

reg <- tempRegistry()
ids <- batchEvalQ(reg, exprs=list(expr), local=FALSE)
print(ids)
res <- submitJobs(reg, ids=ids)
print(res)



## Cleanup
options(oopts)
rm(list=setdiff(ls(envir=globalenv()), ovars), envir=globalenv())
