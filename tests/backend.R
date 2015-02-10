R.utils::use()
use("async")

oopts <- options(warn=1, "async::debug"=FALSE)

## Display current backend() used
obackend <- backend("?")
printf("Original backend: %s\n", obackend)

## Use interactive backend
backend("interactive")
printf("Current backend: %s\n", backend("?"))

a <- 3.14
x %<=% { a }
printf("x=%g\n", x)
stopifnot(x == a)

## Use 'multicore', if available, otherwise 'local'
backend(c("multicore", "local"))
printf("Current backend: %s\n", backend("?"))

y %<=% { 2*a }
printf("y=%g\n", y)
stopifnot(y == 2*a)


## Switch back to the orginal settings
backend(obackend)
printf("Current backend: %s\n", backend("?"))
if (!"covr" %in% loadedNamespaces())
  stopifnot(backend("?") == obackend)

z %<=% { y / 2 }
printf("z=%g\n", z)
stopifnot(z == x)


## Create a backend aliases
backend("spare"=c("multicore-2", "local"))
printf("Current backend: %s\n", backend("?"))

x %<=% { y / 2 } %backend% "interactive"
y %<=% { 2*a } %backend% "local"
z %<=% { y / 2 } %backend% "spare"
printf("x=%g\n", x)
printf("y=%g\n", y)
printf("z=%g\n", z)
stopifnot(x == a)
stopifnot(y == 2*a)
stopifnot(z == x)


## Assert that the original backend is still in use
printf("Current backend: %s\n", backend("?"))
if (!"covr" %in% loadedNamespaces())
  stopifnot(backend("?") == obackend)

## Undo everything
backend(obackend)
printf("Backend was reset to: %s\n", backend("?"))
options(oopts)
