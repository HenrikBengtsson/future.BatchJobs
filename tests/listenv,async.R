source("incl/start.R")

message("*** %<=% to list environments ...")

## - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## Async delayed assignment (infix operator)
## - - - - - - - - - - - - - - - - - - - - - - - - - - - -
z <- listenv()
stopifnot(length(names(z)) == 0)

message("*** %<=% on listenv: Assign by index")
z[[1]] %<=% { 2 }
stopifnot(length(z) == 1)
stopifnot(length(names(z)) == 0)

z[[4]] %<=% { "async!" }
stopifnot(length(z) == 4)
stopifnot(length(names(z)) == 0)

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
fu <- futureOf(u$a)

v <- listenv()
v$a %<=% 2
stopifnot(identical(names(v), "a"))
fv <- futureOf(v$a)
stopifnot(!identical(fu, fv))

fu <- futureOf(u$a)
stopifnot(!identical(fu, fv))

stopifnot(identical(u$a, 1))
stopifnot(identical(v$a, 2))

message("*** %<=% to list environments ... DONE")

source("incl/end.R")
