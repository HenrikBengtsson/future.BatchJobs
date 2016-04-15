# async: A Future for BatchJobs

## Introduction
This package provides a Future API for the [BatchJobs] package by
enhancing the [future] package.  For an introduction how to use
futures in R, please consult the vignettes of the [future] package.
A large fraction of the below text is just an adopted iteration
of what is already documented in [future].


## Asynchronous evaluation
_Asynchronous evaluation_ is a method for evaluating multiple R
expressions in, typically, a parallel or distributed fashion such that
the "observed" total time for computing all values is less that if the
expressions would be evaluated synchronously (sequentially).
For instance, the following evaluation, which is synchronous, takes
about 10 seconds to complete:

```r
> x <- { Sys.sleep(5); 3.14 }
> y <- { Sys.sleep(5); 2.71 }
> x + y
[1] 5.85
```

whereas the following _asynchronous_ evaluation using BatchJobs
futures only takes about 5 seconds to complete since it done in
parallel on multiple cores:

```r
> library('async')
> plan(batchjobs, backend='multicore')
>
> x %<-% { Sys.sleep(5); 3.14 }
> y %<-% { Sys.sleep(5); 2.71 }
> x + y
[1] 5.85
```

### Interrupts
Interrupts such as user interrupts ("Ctrl-C") will only interrupt any
evaluation running in the same R session.  They will not interrupts
the evaluation of asynchronous expressions running in separate R
processes such as those pushed out on a cluster.  This can be useful
when one tries to get the value of a asynchronous evaluation that
took longer than expected causing R to pause.  By hitting Ctrl-C one
can get back to the main prompt and do other futures while waiting for
the long-running evaluation to complete.


## Choosing backend
The asynchronous evaluation done by the [async] package uses the
[BatchJobs] package as a backend for effectuating the computations.
This can be configured using `plan(batchjobs, backend=...)`.
Examples:

* `plan(batchjobs, backend="default")` - use `.BatchJobs.R`
   configuration file, if available. If not, use `"multicore-1"` if
   supported, otherwise use `"local"`
* `plan(batchjobs, backend="multicore")` - parallel processing using
  all available cores on the local machine.
* `plan(batchjobs, backend="multicore-1")` - parallel processing using
  all but one of the available cores on the local machine.
* `plan(batchjobs, backend="local")` - non-parallel processing in a
  separate R process.
* `plan(batchjobs, backend="interactive")` - non-parallel processing
  in the current R session.
* `plan(batchjobs, backend=".BatchJobs.R")` - use `.BatchJobs.R`
  configuration file.

It is possible to specify a set of possible backends,
e.g. `plan(batchjobs, backend=c("multicore", "local"))`.  The first
available/supported backend will be used.

If none of the requested backends work/are supported, the fallback is
always to use the `"local"` which is available on all systems.

To see what the most recently set backend was, use `backend()`.
To reset, use `backend("reset")`
(which is equivalent to `backend("default")`).


### Advanced configuration
For more complicated backends (e.g. clusters), one has to use
BatchJobs-specific configuration files as explained in the Appendix.
The default is to use such configuration files, if available.  To
explicitly use such backend configurations, use
`plan(batchjobs, backend=".BatchJobs.R")`. 


### Backend aliases
It is possible to create aliases for favorite sets of backends.  For
instance,
```r
backend(cluster=c(".BatchJobs.R", "multicore", "local"))
```
creates backend alias `"cluster"` using whatever BatchJobs
configuration file is available with fallback to `"multicore"`
and `"local"`.  After setting an alias it can be specified as:
```r
plan(batchjobs, backend="cluster")
```


### Evaluate asynchronous expression on specific backend
Asynchronous expressions are processed by the default backend as given
by `backend()`.  If another backend should be used to evaluate for a
particular expression, operator `%plan%` can be used.  For example,
```r
a %<-% { Sys.sleep(7); runif(1) } %plan% batchjobs(backend="multicore-2")
b %<-% { Sys.sleep(2); rnorm(1) } %plan% batchjobs(backend="cluster-2")
c %<-% { x <- a*b; Sys.sleep(2); abs(x) }
d <- runif(1)
```
In this case expression `a` will be processed by the `multicore-2`
backend, expression `c` by the `cluster` backend, and expression `c`
by the default backend.

Backend specifications can also be used in nested asynchronous
evaluations:
```r
plan(batchjobs, backend="cluster")
a %<-% { Sys.sleep(7); runif(1) }
c %<-% {
  b %<-% { Sys.sleep(2); rnorm(1) } %plan% batchjobs(backend="multicore=2")
  x <- a*b; Sys.sleep(2); abs(x)
}
d <- runif(1)
```

## Demos and Examples

### Mandelbrot demo
The [future] package provdides a demo of how futures can be used to
calculate a set of Mandelbrot planes.  The demo script is the same
regardless of how the futures are resolved, i.e. in addition to
running that demo using eager, lazy and multicore evaluation
strategies, you can also run it using one of the many BatchJobs
futures.  For instance, the following evaluates the BatchJobs futures
using a "local" backend, i.e. each of them are processed in a seperate
R session:
```r
library(async)
plan(batchjobs, batchjobs="local")
demo("mandelbrot", package="future", ask=FALSE)
```


### Download files in parallel
```r
library('async')
library('R.utils')
repos <- c(CRAN="http://cran.r-project.org",
           Bioc="http://www.bioconductor.org/packages/release/bioc")
urls <- sapply(repos, file.path, "src/contrib/PACKAGES", fsep="/")
files <- new.env()
for (name in names(urls)) {
  files[[name]] %<-% downloadFile(urls[[name]], path=name)
}
str(as.list(files))
```


## Availability
This package is only available via GitHub.  Install in R as:

```s
install.packages('future')
source('http://callr.org/install#HenrikBengtsson/async')
```

## Appendix

### Configuration of backend for parallel / distributed processing
Basic backends can be configured using the `plan(batchjobs, backend=...)`.
For full control, or for more complicated backends such as clusters,
one can use the configuration options available from the BatchJobs
package.  In summary, this type of configuration is done via a
`.BatchJobs.R` configuration file that can reside in either the
current directory or the user's home directory
(this file is only needed on compute nodes if nested asynchronous
calls should also use the same configuration).  These settings
are used by default if available.  They also be explicitly specified
by `plan(batchjobs, backend=".BatchJobs.R")`.

For example, to configure BatchJobs to distribute computations on a
 compute cluster with a TORQUE/PBS job scheduler, you  can use:

let the `.BatchJobs.R` file contain:
```r
cluster.functions <- local({
  makeClusterFunctionsTorque(R.utils::tmpfile('
#PBS -N <%= job.name %>
## merge standard error and output
#PBS -j oe
## direct streams to our logfile
#PBS -o <%= log.file %>
#PBS -l walltime=<%= resources$walltime %>,nodes=<%= resources$nodes %>,vmem=<%= resources$memory %>M
## remove this line if your cluster does not support arrayjobs
#PBS -t 1-<%= arrayjobs %>
  
## Run R:
## we merge R output with stdout from PBS, which gets then logged via -o option
R CMD BATCH --no-save --no-restore "<%= rscript %>" /dev/stdout
  '))
})
```

For further details and examples on how to configure BatchJobs,
see the [BatchJobs configuration] wiki page.


[BatchJobs]: http://cran.r-project.org/package=BatchJobs
[brew]: http://cran.r-project.org/package=brew
[future]: http://cran.r-project.org/package=future
[async]: https://github.com/HenrikBengtsson/async/
[BatchJobs configuration]: https://github.com/tudo-r/BatchJobs/wiki/Configuration

---
Copyright Henrik Bengtsson, 2015-2016

## Installation
R package async is only available via [GitHub](https://github.com/HenrikBengtsson/async) and can be installed in R as:
```r
source('http://callr.org/install#HenrikBengtsson/async')
```

### Pre-release version
To install the pre-release version that is available in branch `develop`, use:
```r
source('http://callr.org/install#HenrikBengtsson/async@develop')
```
This will install the package from source.  



## Software status

| Resource:     | GitHub        | Travis CI     | Appveyor         |
| ------------- | ------------------- | ------------- | ---------------- |
| _Platforms:_  | _Multiple_          | _Linux_       | _Windows_        |
| R CMD check   |  | <a href="https://travis-ci.org/HenrikBengtsson/async"><img src="https://travis-ci.org/HenrikBengtsson/async.svg" alt="Build status"></a> | <a href="https://ci.appveyor.com/project/HenrikBengtsson/async"><img src="https://ci.appveyor.com/api/projects/status/github/HenrikBengtsson/async?svg=true" alt="Build status"></a> |
| Test coverage |                     | <a href="https://coveralls.io/r/HenrikBengtsson/async"><img src="https://coveralls.io/repos/HenrikBengtsson/async/badge.svg?branch=develop" alt="Coverage Status"/></a>   |                  |
