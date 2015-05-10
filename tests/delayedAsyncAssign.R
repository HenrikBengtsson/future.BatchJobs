library("async")

delayedAssign("a", {
  ## This message will be outputted
  cat("Delayed assignment evaluated\n")
  1
})

delayedAsyncAssign("b", {
  ## This message will *not* be outputted, because it
  ## is evaluated in a separate process elsewhere where
  ## the output ends up
  cat("Delayed asynchroneous assignment evaluated\n")
  2
})

cat(sprintf("b=%s\n", b))

## The evaluation of 'a' is evaluated at this point
cat(sprintf("a=%s\n", a))
