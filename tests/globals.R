library("async")
library("R.utils")

findGlobals <- async:::findGlobals

message("Setting up expressions")
a <- 1
exprs <- list(
  A = substitute({ Sys.sleep(5); x <- 0.1 }),
  B = substitute({ Sys.sleep(5); y <- 0.2 }),
  B = substitute({ Sys.sleep(5); z <- a+0.3 })
)


message("Searching for globals")
for (kk in seq_along(exprs)) {
  mprintf("Expression #%d:\n", kk)
  expr <- exprs[[kk]]
  mprint(expr)
  names <- findGlobals(expr)
  mprintf("Globals: %s\n", paste(sQuote(names), collapse=", "))
  mcat("\n")
}
