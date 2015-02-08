library("async")
oopts <- options(warn=1, "async::debug"=FALSE)

task1 <- async({ print("Starting"); y <- 2; print("Finished"); y })
print(task1)

task2 <- async({ print("Starting"); y <- 2; stop("Woops!"); print("Finished"); y })
print(task2)

res1 <- await(task1)
print(res1)
stopifnot(res1 == 2)

res <- tryCatch({
  res2 <- await(task2)
  print(res2)
  res2
}, error = function(ex) {
  print(ex)
  NULL
})
stopifnot(is.null(res))

## Cleanup
options(oopts)
