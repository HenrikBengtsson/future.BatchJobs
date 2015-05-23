library("async")

ovars <- ls(envir=globalenv())
oopts <- options(warn=1, "async::debug"=TRUE)
if (exists("x")) rm(list="x")


## - - - - - - - - - - - - - - - - - - - - - - - - - -
## Variable in global/parent environment
## - - - - - - - - - - - - - - - - - - - - - - - - - -
target <- parseEnvSubset(x, substitute=TRUE)
str(target)
stopifnot(identical(target$envir, environment()),
          target$name == "x", is.na(target$idx), !target$exists)

target <- parseEnvSubset("x", substitute=TRUE)
str(target)
stopifnot(identical(target$envir, environment()),
          target$name == "x", is.na(target$idx), !target$exists)

x <- NULL
target <- parseEnvSubset(x, substitute=TRUE)
str(target)
stopifnot(identical(target$envir, environment()),
          target$name == "x", is.na(target$idx), target$exists)

target <- parseEnvSubset(y, substitute=TRUE)
str(target)
stopifnot(identical(target$envir, environment()),
          target$name == "y", is.na(target$idx), !target$exists)


## - - - - - - - - - - - - - - - - - - - - - - - - - -
## Environment
## - - - - - - - - - - - - - - - - - - - - - - - - - -
message("*** environment")
x <- new.env()

target <- parseEnvSubset(x, substitute=TRUE)
str(target)
stopifnot(identical(target$envir, environment()),
          target$name == "x", is.na(target$idx), target$exists)

target <- parseEnvSubset(x$a, substitute=TRUE)
str(target)
stopifnot(identical(target$envir, x), target$name == "a", is.na(target$idx), !target$exists)

target <- parseEnvSubset("a", envir=x, substitute=TRUE)
str(target)
stopifnot(identical(target$envir, x), target$name == "a", is.na(target$idx), !target$exists)

target <- parseEnvSubset(x[["a"]], substitute=TRUE)
str(target)
stopifnot(identical(target$envir, x), target$name == "a", is.na(target$idx), !target$exists)

target <- parseEnvSubset("a", envir=x, substitute=TRUE)
str(target)
stopifnot(identical(target$envir, x), target$name == "a", is.na(target$idx), !target$exists)

res <- try(target <- parseEnvSubset(x[[1]], substitute=TRUE), silent=TRUE)
stopifnot(inherits(res, "try-error"))

x$a <- 1
target <- parseEnvSubset(x$a, substitute=TRUE)
str(target)
stopifnot(identical(target$envir, x), target$name == "a", is.na(target$idx), target$exists)


## - - - - - - - - - - - - - - - - - - - - - - - - - -
## List environment
## - - - - - - - - - - - - - - - - - - - - - - - - - -
message("*** listenv")
x <- listenv()

target <- parseEnvSubset(x, substitute=TRUE)
str(target)
stopifnot(identical(target$envir, environment()),
          target$name == "x", is.na(target$idx), target$exists)

target <- parseEnvSubset(x$a, substitute=TRUE)
str(target)
stopifnot(identical(target$envir, x), target$name == "a", is.na(target$idx), !target$exists)

target <- parseEnvSubset(x[["a"]], substitute=TRUE)
str(target)
stopifnot(identical(target$envir, x), target$name == "a", is.na(target$idx), !target$exists)

target <- parseEnvSubset("a", envir=x, substitute=TRUE)
str(target)
stopifnot(identical(target$envir, x), target$name == "a", is.na(target$idx), !target$exists)

target <- parseEnvSubset(x[[1]], substitute=TRUE)
str(target)
stopifnot(identical(target$envir, x), target$name == "", target$idx == 1, !target$exists)

target <- parseEnvSubset(x[[2]], substitute=TRUE)
str(target)
stopifnot(identical(target$envir, x), target$name == "", target$idx == 2, !target$exists)

x$a <- 1
target <- parseEnvSubset(x$a, substitute=TRUE)
str(target)
stopifnot(identical(target$envir, x), target$name == "a", target$idx  == 1, target$exists)

target <- parseEnvSubset("a", envir=x, substitute=TRUE)
str(target)
stopifnot(identical(target$envir, x), target$name == "a", target$idx  == 1, target$exists)

stopifnot(x$a == 1)
stopifnot(x[[1]] == 1)

target <- parseEnvSubset(x[[1]], substitute=TRUE)
str(target)
stopifnot(identical(target$envir, x), target$name == "a", target$idx  == 1, target$exists)


x[[3]] <- 3
target <- parseEnvSubset(x[[3]], substitute=TRUE)
str(target)
stopifnot(identical(target$envir, x), target$name == "", target$idx  == 3, target$exists)
stopifnot(x[[3]] == 3)
print(names(x))
stopifnot(identical(names(x), c("a", "", "")))



## - - - - - - - - - - - - - - - - - - - - - - - - - -
## Exception handling
## - - - - - - - - - - - - - - - - - - - - - - - - - -
res <- try(target <- parseEnvSubset(x[[""]], substitute=TRUE), silent=TRUE)
stopifnot(inherits(res, "try-error"))

res <- try(target <- parseEnvSubset("_a", substitute=TRUE), silent=TRUE)
stopifnot(inherits(res, "try-error"))


## Cleanup
options(oopts)
rm(list=setdiff(ls(envir=globalenv()), ovars), envir=globalenv())
