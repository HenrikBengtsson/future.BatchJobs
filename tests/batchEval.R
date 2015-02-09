R.utils::use()
use("async")
use("BatchJobs")

oopts <- options(warn=1, "async::debug"=TRUE)

reg <- async:::tempRegistry()

a <- 1
exprs <- list(
  A = substitute({ Sys.sleep(5); x <- 0.1 }),
  B = substitute({ Sys.sleep(5); y <- 0.2 }),
  B = substitute({ Sys.sleep(5); z <- a+0.3 })
)
ids <- batchEval(reg, exprs=exprs)
mprint(ids)

res <- submitJobs(reg, ids=ids)
mprint(res)

showStatus(reg)

# Wait until all jobs have finished
while(!all(ids %in% findDone(reg))) Sys.sleep(1)


a <- loadResult(reg, id=ids[1])
mprintf("a=%g\n", a)
stopifnot(a == 0.1)

b <- loadResult(reg, id=ids[2])
mprintf("b=%g\n", b)
stopifnot(b == 0.2)

c <- loadResult(reg, id=ids[3])
mprintf("c=%g\n", c)
stopifnot(c == 1.3)

removeRegistry(reg, ask="no"); rm(list="reg")
