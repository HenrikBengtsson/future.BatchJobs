library("async")

ovars <- ls(envir=globalenv())
oopts <- options(warn=1, "async::debug"=TRUE)
obackend <- backend("interactive")


f <- future({
  Sys.sleep(3)
  42L
})

print(f)
print(isResolved(f))
y <- value(f)
print(y)


## Cleanup
backend(obackend)
options(oopts)
rm(list=setdiff(ls(envir=globalenv()), ovars), envir=globalenv())
