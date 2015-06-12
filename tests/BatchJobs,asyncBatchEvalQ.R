library("async")
library("R.utils")

ovars <- ls(envir=globalenv())
oopts <- options(warn=1, "async::debug"=TRUE)
plan(async)
obe <- backend(c("multicore=2", "local"))

message("*** asyncBatchEvalQ()")

message("Creating temporary batch registry")
reg <- async:::tempRegistry()

message("Setting up expressions")
a <- 1
square <- function(x) x^2

exprs <- list(
  A = substitute({ Sys.sleep(5); x <- 0.1 }, env=list()),
  B = substitute({ Sys.sleep(5); y <- 0.2 }, env=list()),
  C = substitute({ Sys.sleep(5); z <- a+0.3 }, env=list()),
  D = substitute({ x <- 1; w <- square(a+x) }, env=list())
)

message("Adding expressions to batch registry")
ids <- asyncBatchEvalQ(reg, exprs=exprs)
mprint(ids)

message("Submitting expressions")
res <- submitJobs(reg, ids=ids)
mprint(res)

message("Current registry status")
showStatus(reg)

message("Wait for jobs to finish")
while(!all(ids %in% findDone(reg))) Sys.sleep(1)

message("Load results")
a <- loadResult(reg, id=ids[1])
mprintf("a=%g\n", a)
stopifnot(a == 0.1)

b <- loadResult(reg, id=ids[2])
mprintf("b=%g\n", b)
stopifnot(b == 0.2)

c <- loadResult(reg, id=ids[3])
mprintf("c=%g\n", c)
stopifnot(c == 1.3)

d <- loadResult(reg, id=ids[4])
mprintf("d=%g\n", d)
stopifnot(d == 4)


message("Protect against large globals (being exported)")
oopts2 <- options(future=async, "async::maxSizeOfGlobals"=100)
a <- 1:100
res <- try(b %<=% { x <- a + 1 })
stopifnot(inherits(res, "try-error"))
options(oopts2)

message("Cleanup")
## Cleanup
options(oopts)
rm(list=setdiff(ls(envir=globalenv()), ovars), envir=globalenv())

message("DONE!")
