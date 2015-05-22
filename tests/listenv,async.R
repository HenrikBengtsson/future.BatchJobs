library("async")
backend("interactive")

ovars <- ls(envir=globalenv())
oopts <- options(warn=1, "async::debug"=TRUE)

## - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## Async delayed assignment (infix operator)
## - - - - - - - - - - - - - - - - - - - - - - - - - - - -
z <- listenv()
stopifnot(is.null(names(z)))

message("*** %<=% on listenv: Assign by index")
z[[1]] %<=% { 2 }
stopifnot(length(z) == 1)
stopifnot(is.null(names(z)))

z[[4]] %<=% { "async!" }
stopifnot(length(z) == 4)
stopifnot(is.null(names(z)))

message("*** %<=% on listenv: Update names")
names(z) <- c("A", "B", "C", "D")
stopifnot(identical(names(z), c("A", "B", "C", "D")))


message("*** %<=% on listenv: Assign by name (existing)")
z$B %<=% TRUE
stopifnot(length(z) == 4)
stopifnot(identical(names(z), c("A", "B", "C", "D")))

y <- as.list(z)
str(y)
stopifnot(length(y) == 4)
stopifnot(identical(names(y), c("A", "B", "C", "D")))


message("*** %<=% on listenv: Potential task name clashes")
u <- listenv()
u$a %<=% 1
stopifnot(identical(names(u), "a"))
tu <- inspect(u$a)

v <- listenv()
v$a %<=% 2
stopifnot(identical(names(v), "a"))
tv <- inspect(v$a)
stopifnot(!identical(tu, tv))

tu <- inspect(u$a)
## FIXME (again)
#stopifnot(!identical(tu, tv))

stopifnot(identical(u$a, 1))
stopifnot(identical(v$a, 2))


## Cleanup
options(oopts)
rm(list=setdiff(ls(envir=globalenv()), ovars), envir=globalenv())
