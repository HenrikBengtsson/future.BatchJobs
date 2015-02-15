library("async")

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

