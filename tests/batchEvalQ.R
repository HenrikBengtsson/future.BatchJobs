library("BatchJobs")
library("async")

tempRegistry <- async:::tempRegistry


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
})
print(expr)

reg <- tempRegistry()
res <- batchExport(reg, li=list(url=url))
ids <- batchEvalQ(reg, exprs=list(expr))
res <- submitJobs(reg, ids=ids)
print(res)

showStatus(reg)
