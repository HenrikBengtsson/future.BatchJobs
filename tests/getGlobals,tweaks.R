library("async")
library("globals")

tweakExpression <- async:::tweakExpression

b <- 2
exprs <- list(
  A = substitute({ a <- b; }, env=list()),
  B = substitute({ a <- b; b <- 1 }, env=list()),  ## Fails to find 'b'
  C = substitute({ a <- 1; a <- 2 }, env=list()),
  D = substitute({ a <<- 1; a <- 2 }, env=list()),
  E = substitute({ a <<- 1 }, env=list())          ## Should not find 'a'
)

truth <- list(
  A = "b",
  B = character(0L),
  C = character(0L),
  D = character(0L),
  E = character(0L)
)

for (kk in seq_along(exprs)) {
  name <- names(exprs)[kk]
  expr <- exprs[[kk]]
  cat(sprintf("Expression #%d ('%s'):", kk, name))
  print(expr)
  globals <- getGlobals(expr, tweak=tweakExpression)
  globals <- cleanup(globals)
  str(globals)
  stopifnot(identical(names(globals), truth[[name]]))
}
