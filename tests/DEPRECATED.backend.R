source("incl/start.R")

message("*** backend() ...")

## Display current backend() used
obackend <- backend(NULL)
mprintf("Original backend: %s\n", obackend)

## Use interactive backend
backend("interactive")
mprintf("Current backend: %s\n", backend(NULL))

a <- 3.14
x %<-% { a }
mprintf("x=%g\n", x)
stopifnot(x == a)

## Use 'multicore', if available, otherwise 'local'
if (fullTest) {
  backend(c("multicore", "local"))
} else {
  backend("local")
}
mprintf("Current backend: %s\n", backend(NULL))

y %<-% { 2*a }
mprintf("y=%g\n", y)
stopifnot(y == 2*a)


## Switch back to the orginal settings
backend(obackend)
mprintf("Current backend: %s\n", backend(NULL))
if (!"covr" %in% loadedNamespaces())
  stopifnot(backend(NULL) == obackend)

z %<-% { y / 2 }
mprintf("z=%g\n", z)
stopifnot(z == x)


## Create a backend aliases
if (fullTest) {
  backend(spare=c("multicore-2", "local"))
} else {
  backend(spare="local")
}
mprintf("Current backend: %s\n", backend(NULL))

x %<-% { y / 2 } %plan% batchjobs(backend="interactive")
y %<-% { 2*a } %plan% batchjobs(backend="local")
z %<-% { y / 2 } %plan% batchjobs(backend="spare")
mprintf("x=%g\n", x)
mprintf("y=%g\n", y)
mprintf("z=%g\n", z)
stopifnot(x == a)
stopifnot(y == 2*a)
stopifnot(z == x)

## Assert that the original backend is still in use
mprintf("Current backend: %s\n", backend(NULL))
if (!"covr" %in% loadedNamespaces())
  stopifnot(backend(NULL) == obackend)


## Specify BatchJobs config file as backend
path <- system.file("etc", package="BatchJobs")
pathname <- file.path(path, "BatchJobs_global_config.R")
if (file_test("-f", pathname)) {
  backend(pathname)
  mprintf("Current backend: %s\n", backend(NULL))

  backend(cluster=pathname)
  backend('cluster')
  mprintf("Current backend: %s\n", backend(NULL))
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

mprintf("Backend was reset to: %s\n", backend(NULL))

message("*** backend() - exceptions ...")

options(future.backend.onUnknown="warn")
res <- tryCatch({
  backend("multicore=1")
}, warning = function(w) w)
stopifnot(inherits(res, "warning"))
options(future.backend.onUnknown=NULL)

res <- try(backend("list", 1), silent=FALSE)
print(res)
stopifnot(inherits(res, "try-error"))

res <- try(backend("list", a=1), silent=FALSE)
print(res)
stopifnot(inherits(res, "try-error"))

res <- try(backend("multicore=0"), silent=FALSE)
print(res)
stopifnot(inherits(res, "try-error"))

res <- try(backend("multicore--1"), silent=FALSE)
print(res)
stopifnot(inherits(res, "try-error"))

## Configuration file without cluster function
pathname <- ".BatchJobs.R"
cat("message('.BatchJobs.R here!')", file=pathname)
res <- backend(".BatchJobs.R")
print(res)
file.remove(pathname)

message("*** backend() - exceptions ... DONE")

message("*** backend() ... DONE")


source("incl/end.R")
