R.utils::use()
use("async")

## - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## Sequential evaluation
## - - - - - - - - - - - - - - - - - - - - - - - - - - - -
t0 <- Sys.time()

v1 <- {
  cat("Calculation #1: ")
  for (ii in 1:10) { cat("."); Sys.sleep(0.5) }
  cat(" [100%]\n")
  1
}

a <- 2
v2 <- {
  cat("Calculation #2: ")
  for (ii in 1:10) { cat("."); Sys.sleep(0.5) }
  cat(" [100%]\n")
  a
}

v3 <- {
  cat("Calculation #3: ")
  for (ii in 1:10) { cat("."); Sys.sleep(0.5) }
  cat(" [100%]\n")
  3
}

mprintf("v1=%s\n", v1)
mprintf("v2=%s\n", v2)
mprintf("v3=%s\n", v3)

mprintf("Total time: %.1f s\n", Sys.time() - t0)



## - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## Asynchroneous evaluation
## - - - - - - - - - - - - - - - - - - - - - - - - - - - -
t0 <- Sys.time()

v1 %<-% {
  cat("Calculation #1: ")
  for (ii in 1:10) { cat("."); Sys.sleep(0.5) }
  cat(" [100%]\n")
  1
}

a <- 2
v2 %<-% {
  cat("Calculation #2: ")
  for (ii in 1:10) { cat("."); Sys.sleep(0.5) }
  cat(" [100%]\n")
  a
}

v3 %<-% {
  cat("Calculation #3: ")
  for (ii in 1:10) { cat("."); Sys.sleep(0.5) }
  cat(" [100%]\n")
  3
}

mprintf("v1=%s\n", v1)
mprintf("v2=%s\n", v2)
mprintf("v3=%s\n", v3)

mprintf("Total time: %.1f s\n", Sys.time() - t0)

