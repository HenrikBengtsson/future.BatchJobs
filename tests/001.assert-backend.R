source("incl/start.R")

message("*** Assert backends ...")

mcat("Backends to be tested:\n")
mprint(TEST_BACKENDS)

env <- new.env()
lenv <- listenv()
for (be in TEST_BACKENDS) {
  message("*** Backend: ", be)
  backend(be)

  message(" - Future and promise")
  f <- async(be)
  print(f)
  v <- await(f)
  print(v)
  stopifnot(v == be)

  message(" - Async evaluation (current environment)")
  a %<=% be
  print(a)
  stopifnot(a == be)

  message(" - Async evaluation (environment)")
  env$b %<=% be
  print(env$b)
  stopifnot(env$b == be)

  message(" - Async evaluation (list environment - name)")
  lenv$b %<=% be
  print(lenv$b)
  stopifnot(lenv$b == be)

  message(" - Async evaluation (list environment - index)")
  lenv[[3]] %<=% be
  print(lenv[[3]])
  stopifnot(lenv[[3]] == be)
}

message("*** Assert backends ... DONE")

source("incl/end.R")
