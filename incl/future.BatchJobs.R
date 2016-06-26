## Use local BatchJobs futures
plan(batchjobs_local)

## A global variable
a <- 1

v %<-% {
  b <- 3
  c <- 2
  a * b * c
}

print(v)
