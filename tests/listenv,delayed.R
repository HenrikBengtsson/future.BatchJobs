library("async")
## Make sure not to clash with R.utils
##`%<-%` <- async::`%<-%`

ovars <- ls(envir=globalenv())
oopts <- options(warn=1, "async::debug"=TRUE)

## - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## Delayed assignment (infix operator)
## - - - - - - - - - - - - - - - - - - - - - - - - - - - -
message("*** %<-% on listenv: Assign by name")
z <- listenv()
z$a %<-% { 1 }
print(names(z))
stopifnot(length(z) == 1L)
stopifnot(identical(names(z), "a"))

z$c %<-% { 3 }
print(names(z))
stopifnot(length(z) == 2L)
stopifnot(identical(names(z), c("a", "c")))

message("*** %<-% on listenv: Assign by index")
z <- listenv()
z[[1]] %<-% { 1 }
z[[3]] %<-% { "Hello world!" }
print(names(z))
stopifnot(is.null(names(z)))
stopifnot(length(z) == 3)

message("*** %<-% on listenv: Update names")
names(z) <- c("a", "b", "c")
stopifnot(identical(names(z), c("a", "b", "c")))

message("*** %<-% on listenv: Extract list")
y <- as.list(z)
str(y)
stopifnot(length(y) == 3)
stopifnot(identical(names(y), c("a", "b", "c")))

message("*** %<-% on listenv: Assign by name (new)")
z$d %<-% TRUE
print(names(z))
stopifnot(identical(names(z), c("a", "b", "c", "d")))

message("*** %<-% on listenv: Assign by name (existing)")
z$b %<-% TRUE
print(names(z))
stopifnot(identical(names(z), c("a", "b", "c", "d")))


message("*** %<-% on listenv: Check for task name clashes")
## Potential task name clashes
u <- listenv()
v <- listenv()
u$a %<-% 1
v$a %<-% 2
stopifnot(identical(u$a, 1))
stopifnot(identical(v$a, 2))


## Cleanup
options(oopts)
rm(list=setdiff(ls(envir=globalenv()), ovars), envir=globalenv())
