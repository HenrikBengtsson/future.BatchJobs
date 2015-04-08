library("async")
library("R.utils")

ovars <- ls(envir=globalenv())
oopts <- options(warn=1, "async::debug"=FALSE)

findGlobals <- async:::findGlobals
getGlobals <- async:::getGlobals

## WORKAROUND: Avoid problem reported in testthat Issue #229, which
## causes covr::package_coverage() to given an error. /HB 2015-02-16
rm(list=c("a", "b", "c", "x", "y", "z", "square",
          "pathname", "url", "filename", "%<-%"))


message("Setting up expressions")
exprs <- list(
  A = substitute({ Sys.sleep(1); x <- 0.1 }, env=list()),
  B = substitute({ y <- 0.2 }, env=list()),
  C = substitute({ z <- a+0.3 }, env=list()),
  D = substitute({ pathname <- file.path(dirname(url), filename) }, env=list()),
  E = substitute({ b %<-% c }, env=list()),
  F = substitute({
    a %<-% { runif(1) }
    b %<-% { rnorm(1) }
    x <- a*b; abs(x)
  }, env=list()),
  G = substitute({
    a %<=% { runif(1) }
    b %<=% { rnorm(1) }
    x <- a*b; abs(x)
  }, env=list()),
  H = substitute({
    y <- square(a)
  }, env=list()),
  I = substitute({
    b <- a
    a <- 1
  }, env=list())
)

atleast <- list(
  A = c(),
  B = c(),
  C = c("a"),
  D = c("filename"),
  E = c("c"),
  F = c(),
  G = c(),
  H = c("a", "square"),
  I = c() ## FIXME: Should be c("a"), cf. Issue #5.
)

not <- list(
  A = c("x"),
  B = c("y"),
  C = c("z"),
  D = c("pathname"),
  E = c("b"),
  F = c("a", "b", "x"),
  G = c("a", "b", "x"),
  H = c(),
  I = c()
)


## Define globals
a <- 3.14
c <- 2.71
square <- function(x) x^2
filename <- "index.html"
# Yes, pretend we forget 'url'

message("Find for globals")
for (kk in seq_along(exprs)) {
  key <- names(exprs)[kk]
  expr <- exprs[[key]]
  mprintf("Expression #%d ('%s'):\n", kk, key)
  mprint(expr)

  names <- findGlobals(expr)
  mprintf("Globals: %s\n", paste(sQuote(names), collapse=", "))
  stopifnot(all(atleast[[key]] %in% names))
  stopifnot(!any(names %in% not[[key]]))

  globals <- getGlobals(expr)
  mprintf("Globals: %s\n", paste(sQuote(names(globals)), collapse=", "))
  stopifnot(all(atleast[[key]] %in% names(globals)))
  stopifnot(!any(names(globals) %in% not[[key]]))
  mstr(globals)

  mcat("\n")
}


## Cleanup
options(oopts)
rm(list=setdiff(ls(envir=globalenv()), ovars), envir=globalenv())
