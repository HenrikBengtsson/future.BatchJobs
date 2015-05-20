library("async")

ovars <- ls(envir=globalenv())
oopts <- options(warn=1, "async::debug"=TRUE)

backend("local")

x <- AsyncListEnv(length=3L)
names(x) <- c("a", "b", "c")
print(x)

x[[1]] %<=% { 1 }
x[[2]] %<=% { stop("Wow!"); 2 }
x[[3]] %<=% { list(foo=3, bar=letters) }
print(x)


## Cleanup
options(oopts)
rm(list=setdiff(ls(envir=globalenv()), ovars), envir=globalenv())
