source("incl/start.R")

message("*** batchjobs() ...")

## An asynchroneous future without globals
f <- batchjobs({ x <- 1 })
print(f)
v <- value(f)
print(v)
stopifnot(v == 1)


## An asynchroneous future with globals
a <- 2
f <- batchjobs({ x <- a })
print(f)
v <- value(f)
print(v)
stopifnot(v == a)


## An asynchroneous future with errors
f <- batchjobs({ x <- 5; stop("Woops!"); x })
print(f)
v <- value(f, signal=FALSE)
print(v)

res <- try(value(f), silent=TRUE)
print(res)
stopifnot(inherits(res, "try-error"))

## Error is repeated
res <- try(value(f), silent=TRUE)
print(res)
stopifnot(inherits(res, "try-error"))


message("*** future() w/ plan(batchjobs) ... OK")

message("- future() / value() ...")
f <- future({
  42L
})
print(resolved(f))
v <- value(f)
print(v)
stopifnot(v == 42L)

message("- future assignment without globals ...")
v %<=% {
  42L
}
print(v)
stopifnot(v == 42L)

message("- future assignment with globals ...")
a <- 3.14
v %<=% { x <- a }
print(v)
stopifnot(v == a)

message("- future assignment with error ...")
v %<=% {
  x <- 3
  stop("Woops!")
  x
}

res <- try(print(v), silent=TRUE)
print(res)
stopifnot(inherits(res, "try-error"))

## Error is repeated
res <- try(print(v), silent=TRUE)
print(res)
stopifnot(inherits(res, "try-error"))


message("*** batchjobs() ... OK")

#source("incl/end.R")
