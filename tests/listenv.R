library("async")
## Make sure not to clash with R.utils
`%<-%` <- async::`%<-%`

ovars <- ls(envir=globalenv())
oopts <- options(warn=1, "async::debug"=FALSE)

x <- listenv()
print(length(x))
print(names(x))
stopifnot(length(x) == 0)

x$a <- 1
print(length(x))
print(names(x))
stopifnot(length(x) == 1)
stopifnot(identical(names(x), c("a")))
stopifnot(x$a == 1, is.null(x$b))

x$b <- 2
print(length(x))
print(names(x))
stopifnot(length(x) == 2)
stopifnot(identical(names(x), c("a", "b")))
stopifnot(x$b == 2)

x$a <- 0
print(length(x))
print(names(x))
stopifnot(length(x) == 2)
stopifnot(identical(names(x), c("a", "b")))
stopifnot(x[["a"]] == 0)

x$"a" <- 1
print(length(x))
print(names(x))
stopifnot(length(x) == 2)
stopifnot(identical(names(x), c("a", "b")))
stopifnot(x$a == 1)

x[["a"]] <- 0
print(length(x))
print(names(x))
stopifnot(length(x) == 2)
stopifnot(identical(names(x), c("a", "b")))


key <- "b"
x[[key]] <- 3
print(length(x))
print(names(x))
stopifnot(length(x) == 2)
stopifnot(identical(names(x), c("a", "b")))
stopifnot(x$b == 3, x[["b"]] == 3, x[[key]] == 3)

x[[3]] <- 3.14
print(length(x))
print(names(x))
stopifnot(length(x) == 3)
stopifnot(identical(names(x), c("a", "b", "")))
stopifnot(x[[3]] == 3.14)

names(x) <- c("a", "b", "c")
stopifnot(length(x) == 3)
stopifnot(identical(names(x), c("a", "b", "c")))
stopifnot(x[[3]] == 3.14, x[["c"]] == 3.14, x$c == 3.14)

x <- listenv()
for (ii in 1:3) {
  x[[ii]] <- letters[ii]
}
names(x) <- sprintf("item%d", seq_along(x))
y <- as.list(x)
str(y)
stopifnot(identical(names(y), c("item1", "item2", "item3")))
stopifnot(y[[1]] == "a", y[[2]] == "b", y[[3]] == "c")
x[[2]] <- "B"
stopifnot(x$item2 == "B")


x <- listenv()
x[[1]] <- { 1 }
x[[3]] <- { "Hello world!" }
stopifnot(length(x) == 3)
stopifnot(identical(seq_along(x), seq_len(length(x))))
names(x) <- c("a", "b", "c")
x$b <- TRUE
stopifnot(x[[1]] == 1)
stopifnot(x[[2]] == TRUE)
y <- as.list(x)
str(y)
stopifnot(length(y) == 3)


## Mixed names and indices
x <- listenv()
x$a <- 1
x[[3]] <- 3
print(names(x))
stopifnot(identical(names(x), c("a", "", "")))

# First element (should be named 'a')
var <- get_variable(x, "a")
stopifnot(var == "a")
var <- get_variable(x, 1)
stopifnot(var == "a")

# Third element (should be a temporary name)
var <- get_variable(x, 3)
stopifnot(var != "c")
names(x) <- c("a", "b", "c")
var <- get_variable(x, 3)
stopifnot(var != "c")
var <- get_variable(x, "c")
stopifnot(var != "c")

## Second element (should become 'b', because it was never used
#                  before it was "named" 'b')
x$b <- 2
var <- get_variable(x, 2)
stopifnot(var == "b")
var <- get_variable(x, "b")
stopifnot(var == "b")


## Names where as.integer(names(x)) are integers
x <- listenv()
x[["1"]] <- 1
x[["3"]] <- 3
print(names(x))
stopifnot(identical(names(x), c("1", "3")))



## - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## Delayed assignment (infix operator)
## - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## By names
z <- listenv()
z$a %<-% { 1 }
z$c %<-% { 3 }
print(names(z))
## FIXME: The infix operator does not assign names. /HB 2015-04-20
##stopifnot(identical(names(z), c("a", "c")))

## By indices
z <- listenv()
z[[1]] %<-% { 1 }
z[[3]] %<-% { "Hello world!" }
print(names(z))
stopifnot(is.null(names(z)))
stopifnot(length(z) == 3)

## Add names
names(z) <- c("a", "b", "c")
z$b %<-% TRUE
y <- as.list(z)
str(y)
stopifnot(length(y) == 3)
stopifnot(identical(names(y), c("a", "b", "c")))



## - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## Async delayed assignment (infix operator)
## - - - - - - - - - - - - - - - - - - - - - - - - - - - -
z <- listenv()
z[[1]] %<=% { 2 }
z[[4]] %<=% { "async!" }
stopifnot(length(z) == 4)
names(z) <- c("A", "B", "C", "D")
z$b %<=% TRUE
y <- as.list(z)
str(y)
stopifnot(length(y) == 4)


## Cleanup
options(oopts)
rm(list=setdiff(ls(envir=globalenv()), ovars), envir=globalenv())
