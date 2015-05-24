backend(c("multicore", "local"))

## Setup three expressions
exprs <- list(
  A = substitute({ x <- 0.1 }, env=list()),
  B = substitute({ y <- 0.2 }, env=list()),
  C = substitute({ z <- a+0.3 }, env=list())
)
str(exprs)

## Evaluate expressions asynchronously

# Define global 'a' used in expression 'C'
a <- 1

lenv <- asyncEvalQ(exprs=exprs)
message("Number of async evaluations: ", length(lenv))
message("Async evaluations names: ", paste(sQuote(names(lenv)), collapse=", "))

message("Values/Results:")

## Sleep until value for expression 'B' is available
value <- lenv$B
message("Value 'B': ", value)

## Sleep until all results are available
values <- as.list(lenv)
str(values)
