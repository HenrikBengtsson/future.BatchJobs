source("incl/start.R")

message("*** async() ...")

## An asynchroneous future without globals
f <- async({ x <- 1 })
print(f)
v <- value(f)
print(v)
stopifnot(v == 1)


## An asynchroneous future with globals
a <- 2
f <- async({ x <- a })
print(f)
v <- value(f)
print(v)
stopifnot(v == a)


## An asynchroneous future with errors
f <- async({ x <- 3; stop("Woops!"); x })
print(f)
v <- value(f, onError="return")
print(v)

res <- tryCatch({
  v <- value(f)
  print(v)
  v
}, error = function(ex) {
  cat(ex$message)
  NULL
})
stopifnot(is.null(res))


message("*** async() ... OK")

source("incl/end.R")
