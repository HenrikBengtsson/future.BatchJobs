R.utils::use()
use("async")

ovars <- ls(envir=globalenv())
oopts <- options(warn=1, "async::debug"=TRUE)
obe <- backend(c("multicore=2", "local"))

message("*** asyncEvalQ()")

message("Setting up expressions")
a <- 1
square <- function(x) x^2

exprs <- list(
  A = substitute({ x <- 0.1 }, env=list()),
  B = substitute({ y <- 0.2 }, env=list()),
  C = substitute({ Sys.sleep(5); z <- a+0.3 }, env=list()),
  D = substitute({ x <- 1; w <- square(a+x) }, env=list())
)
mstr(exprs)

message("Evaluating expressions asynchronously")
env <- asyncEvalQ(exprs=exprs)
mprintf("Number of async evaluations: %d\n", length(env))
mprintf("Names async evaluations, iff any: %s\n", paste(sQuote(names(env)), collapse=", "))
mcat("Values/Results:\n")
values <- as.list(env)
mstr(values)


## Cleanup
options(oopts)
rm(list=setdiff(ls(envir=globalenv()), ovars), envir=globalenv())
