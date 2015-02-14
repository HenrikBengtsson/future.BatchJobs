library("async")
library("R.utils")

findGlobals <- async:::findGlobals
getGlobals <- async:::getGlobals

message("Setting up expressions")
exprs <- list(
  A = substitute({ Sys.sleep(5); x <- 0.1 }),
  B = substitute({ y <- 0.2 }),
  C = substitute({ z <- a+0.3 }),
  D = substitute({ pathname <- file.path(dirname(url), filename) }),
  E = substitute({ b %<-% c })
)

atleast <- list(
  A = c(),
  B = c(),
  C = c("a"),
  D = c("filename"),
  E = c("c")
)

not <- list(
  A = c("x"),
  B = c("y"),
  C = c("z"),
  D = c("pathname"),
  E = c("b")
)


## Define globals
a <- 3.14
c <- 2.71
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
