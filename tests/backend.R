R.utils::use()
use("async")

ovars <- ls(envir=globalenv())
oopts <- options(warn=1, "async::debug"=FALSE)

## Display current backend() used
obackend <- backend(NULL)
printf("Original backend: %s\n", obackend)

## Use interactive backend
backend("interactive")
printf("Current backend: %s\n", backend(NULL))

a <- 3.14
x %<=% { a }
printf("x=%g\n", x)
stopifnot(x == a)

## Use 'multicore', if available, otherwise 'local'
backend(c("multicore", "local"))
printf("Current backend: %s\n", backend(NULL))

y %<=% { 2*a }
printf("y=%g\n", y)
stopifnot(y == 2*a)


## Switch back to the orginal settings
backend(obackend)
printf("Current backend: %s\n", backend(NULL))
if (!"covr" %in% loadedNamespaces())
  stopifnot(backend(NULL) == obackend)

z %<=% { y / 2 }
printf("z=%g\n", z)
stopifnot(z == x)


## Create a backend aliases
backend("spare"=c("multicore-2", "local"))
printf("Current backend: %s\n", backend(NULL))

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
printf("Current backend: %s\n", backend(NULL))
if (!"covr" %in% loadedNamespaces())
  stopifnot(backend(NULL) == obackend)


## Specify BatchJobs config file as backend
path <- system.file("etc", package="BatchJobs")
pathname <- file.path(path, "BatchJobs_global_config.R")
if (file_test("-f", pathname)) {
  backend(pathname)
  printf("Current backend: %s\n", backend(NULL))

  backend(cluster=pathname)
  backend('cluster')
  printf("Current backend: %s\n", backend(NULL))
}

print(backend(".BatchJobs.R"))
print(backend("interactive"))
print(backend("local"))
print(backend("multicore"))
print(backend("multicore=1"))
print(backend("multicore-1"))
print(backend("multicore=999"))
print(backend("aliases"))
print(backend("default"))
print(backend("reset"))


## Undo everything
backend(obackend)
printf("Backend was reset to: %s\n", backend(NULL))

## Cleanup
options(oopts)
rm(list=setdiff(ls(envir=globalenv()), ovars), envir=globalenv())
