R.utils::use()
use("async")

message("*** asyncEvalQ()")

oopts <- options(warn=1, "async::debug"=FALSE)

message("Setting up expressions")
a <- 1
exprs <- list(
  A = substitute({ x <- 0.1 }),
  B = substitute({ y <- 0.2 }),
  B = substitute({ z <- a+0.3 })
)
mstr(exprs)

message("Evaluating expressions asynchronously")
env <- asyncEvalQ(exprs=exprs)
mprintf("Number of async evaluations: %d\n", length(env))
mprintf("Names async evaluations, iff any: %s\n", paste(sQuote(names(env)), collapse=", "))
mcat("Values/Results:\n")
values <- as.list(env)
mstr(values)

## Undo
options(oopts)
