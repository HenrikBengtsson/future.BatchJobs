source("incl/start.R")

message("*** Asynchronous assignment to environments ...")

env <- new.env()

env$a %<=% { 1 }
env[["b"]] %<=% { 2 }
key <- "c"
env[[key]] %<=% { 3 }

mprint(ls(envir=env))
mprintf("env$a=%s\n", env$a)
mprintf("env$b=%s\n", env$b)
mprintf("env$c=%s\n", env$c)
stopifnot(env$a == 1, env$b == 2, env$c == 3)

message("*** Asynchronous assignment to environments ... DONE")

source("incl/end.R")
