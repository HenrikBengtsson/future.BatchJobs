("async")
## Make sure not to clash with R.utils
`%<-%` <- async::`%<-%`
backend("local")

ovars <- ls(envir=globalenv())
oopts <- options(warn=1, "async::debug"=FALSE)

## - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## Delayed assignment (infix operator)
## - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## Assign by name
z <- listenv()
z$a %<-% { 1 }
z$c %<-% { 3 }
print(names(z))
stopifnot(identical(names(z), c("a", "c")))

## Assign by index
z <- listenv()
z[[1]] %<-% { 1 }
z[[3]] %<-% { "Hello world!" }
print(names(z))
stopifnot(is.null(names(z)))
stopifnot(length(z) == 3)

## Add names
names(z) <- c("a", "b", "c")
stopifnot(identical(names(z), c("a", "b", "c")))

## Extract as list
y <- as.list(z)
str(y)
stopifnot(length(y) == 3)
stopifnot(identical(names(y), c("a", "b", "c")))

## Assign by new name
z$d %<-% TRUE
print(names(z))
stopifnot(identical(names(z), c("a", "b", "c", "d")))

## Assign by existing name
z$b %<-% TRUE
print(names(z))
stopifnot(identical(names(z), c("a", "b", "c", "d")))

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
