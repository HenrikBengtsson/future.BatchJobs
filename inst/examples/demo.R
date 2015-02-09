R.utils::use()
use("async")

## - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## Sequential evaluation
## - - - - - - - - - - - - - - - - - - - - - - - - - - - -
v1 <- { x <- 1 }

a <- 2
v2 <- { x <- a }

v3 <- {
  library("R.utils")
  mprintf("Processing: ")
  for (ii in 1:10) { mprintf("."); Sys.sleep(0.5) }
  mprintf(" [100%%]\n")
  3
}

mprintf("v1=%s\n", v1)
mprintf("v2=%s\n", v2)
mprintf("v3=%s\n", v3)



## - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## Asynchroneous evaluation
## - - - - - - - - - - - - - - - - - - - - - - - - - - - -
v1 %<-||% { x <- 1 }

a <- 2
v2 %<-||% { x <- a }

v3 %<-||% {
  library("R.utils")
  mprintf("Processing: ")
  for (ii in 1:10) { mprintf("."); Sys.sleep(0.5) }
  mprintf(" [100%%]\n")
  3
}

mprintf("v1=%s\n", v1)
mprintf("v2=%s\n", v2)
mprintf("v3=%s\n", v3)
