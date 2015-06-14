source("incl/start.R")

message("*** batchEvalQ() ...")

## Global string, which should be found instead of base::url().
url <- "http://www.r-project.org"

message("*** Function")
fun <- function(...) {
  cat("URL:\n")
  print(url)
##  stopifnot(is.character(url), identical(url, "http://www.r-project.org"))
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
##  stopifnot(is.character(url), identical(url, "http://www.r-project.org"))
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


message("*** batchEvalQ() ... DONE")

source("incl/end.R")
