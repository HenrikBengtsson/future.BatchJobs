source("incl/start.R")
library("listenv")

message("*** asyncBatchEvalQ() ...")

message("Creating temporary batch registry")
reg <- tempRegistry()

message("Setting up expressions")
g <- 1
square <- function(x) x^2
a <- 1
.b <- 2
`%foo%` <- 3

exprs <- list(
  A = substitute({ Sys.sleep(1); x <- 0.1 }, env=list()),
  B = substitute({ Sys.sleep(1); y <- 0.2 }, env=list()),
  C = substitute({ Sys.sleep(1); z <- g+0.3 }, env=list()),
  D = substitute({ x <- 1; w <- square(g+x) }, env=list()),
  E = substitute({ x <- listenv(); x$g <- g; x[[1]] }, env=list()),
  F = substitute({ w <- a * .b * `%foo%` }, env=list())
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

message(" - Expression #1")
a <- loadResult(reg, id=ids[1])
mprintf("a=%g\n", a)
stopifnot(a == 0.1)

message(" - Expression #2")
b <- loadResult(reg, id=ids[2])
mprintf("b=%g\n", b)
stopifnot(b == 0.2)

message(" - Expression #3")
c <- loadResult(reg, id=ids[3])
mprintf("c=%g\n", c)
stopifnot(c == 1.3)

message(" - Expression #4")
d <- loadResult(reg, id=ids[4])
mprintf("d=%g\n", d)
stopifnot(d == 4)

message(" - Expression #5")
e <- loadResult(reg, id=ids[5])
mprintf("e=%g\n", e)
stopifnot(e == 1)

message(" - Expression #6")
f <- loadResult(reg, id=ids[6])
mprintf("f=%g\n", f)
stopifnot(f == 6)


message("asyncBatchEvalQ() with explicit globals")
reg2 <- tempRegistry(backend="local")
ids <- asyncBatchEvalQ(reg2, exprs=exprs["D"], globals=list(a=8))
mprint(ids)

message("Protect against large globals (being exported)")
oopts2 <- options("async::maxSizeOfGlobals"=100)
a <- 1:100
res <- try(b %<=% { x <- a + 1 })
stopifnot(inherits(res, "try-error"))
options(oopts2)

message("*** asyncBatchEvalQ() ... DONE")

source("incl/end.R")
