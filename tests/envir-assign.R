R.utils::use()
use("async")

oopts <- options(warn=1, "async::debug"=FALSE)

backend("interactive")

message("** Non-asynchronous assignment to environment")
env <- new.env()

env$a %<-% { 1 }
env[["b"]] %<-% { 2 }
key <- "c"
env[[key]] %<-% { 3 }

print(ls(envir=env))
printf("env$a=%s\n", env$a)
printf("env$b=%s\n", env$b)
printf("env$c=%s\n", env$c)
stopifnot(env$a == 1, env$b == 2, env$c == 3)


message("** Asynchronous assignment to environment")
env <- new.env()

env$a %<=% { 1 }
env[["b"]] %<=% { 2 }
key <- "c"
env[[key]] %<=% { 3 }

print(ls(envir=env))
printf("env$a=%s\n", env$a)
printf("env$b=%s\n", env$b)
printf("env$c=%s\n", env$c)
stopifnot(env$a == 1, env$b == 2, env$c == 3)


## Cleanup
options(oopts)
