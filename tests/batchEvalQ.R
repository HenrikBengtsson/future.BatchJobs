R.utils::use()
use("async")

message("*** backEvalQ()")

oopts <- options(warn=1, "async::debug"=TRUE)

message("Creating temporary batch registry")
reg <- async:::tempRegistry()

message("Setting up expressions")
a <- 1
exprs <- list(
  A = substitute({ Sys.sleep(5); x <- 0.1 }),
  B = substitute({ Sys.sleep(5); y <- 0.2 }),
  B = substitute({ Sys.sleep(5); z <- a+0.3 })
)

message("Adding expressions to batch registry")
ids <- batchEvalQ(reg, exprs=exprs)
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

message("Remove registry")
removeRegistry(reg, ask="no"); rm(list="reg")


## Undo
options(oopts)

