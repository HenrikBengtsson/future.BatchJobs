library("async")
oopts <- options("future"=lazyfuture)

f <- future({
  42L
})

print(isResolved(f))
y <- value(f)
print(y)
stopifnot(y == 42L)


## Cleanup
options(oopts)
