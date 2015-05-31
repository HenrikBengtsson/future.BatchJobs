library("async")

f <- lazyfuture({
  42L
})
stopifnot(inherits(f, "LazyFuture"))

print(isResolved(f))
y <- value(f)
print(y)
stopifnot(y == 42L)
