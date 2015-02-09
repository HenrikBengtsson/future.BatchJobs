R.utils::use()
use("async")

oopts <- options(warn=1, "async::debug"=TRUE)

t0 <- Sys.time()
print(t0)

## Evaluate three expressions in parallel that each
## takes at least 5 seconds to complete
t1 <- async({ x <- 1; Sys.sleep(5); x })
print(t1)
dt1 <- Sys.time() - t0
print(dt1)

a <- 2
t2 <- async({ Sys.sleep(5); x <- a })
print(t2)
dt2 <- Sys.time() - t0
print(dt2)

t3 <- async({ x <- 3; Sys.sleep(5); stop("Woops!"); x })
print(t3)
dt3 <- Sys.time() - t0
print(dt3)


## Check results
v1 <- await(t1)
print(v1)
stopifnot(v1 == 1)
dt11 <- Sys.time() - t0
print(dt11)

v2 <- await(t2)
print(v2)
stopifnot(v2 == a)
dt12 <- Sys.time() - t0
print(dt12)

res <- tryCatch({
  v3 <- await(t3)
  print(v3)
  v3
}, error = function(ex) {
  cat(ex$message)
  cat("\nTEST OK!\n")
  NULL
})
stopifnot(is.null(res))
dt13 <- Sys.time() - t0
print(dt13)

dt_all <- Sys.time() - t0
print(dt_all)

## Cleanup
options(oopts)
