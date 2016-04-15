plan(batchjobs, backend=c("multicore", "local"))

## A global variable
a <- 1

## Three asynchronous evaluations
A %<-% { x <- 0.1 }
B %<-% { y <- 0.2 }
C %<-% { z <- a+0.3 }

## Sleep until 'C' is available
message("Value 'C': ", C)

## Sleep until 'A' is available
message("Value 'A': ", A)

## Sleep until 'C' is available
message("Value 'B': ", B)
