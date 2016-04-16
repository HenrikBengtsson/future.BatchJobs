# future.BatchJobs: A Future for BatchJobs

## Introduction
The [future.BatchJobs] package provides [BatchJobs] futures according to
the Future API defined by the [future] package.
This means that all of the BatchJobs machinery can be
utilized using futures, e.g.
```r
> library('future.BatchJobs')
> plan(batchjobs)
>
> x %<-% { Sys.sleep(5); 3.14 }
> y %<-% { Sys.sleep(5); 2.71 }
> x + y
[1] 5.85
```
For an introduction how to use futures in R, please consult the
vignettes of the [future] package.


## Choosing BatchJobs backend
The future.BatchJobs package implements a generic future wrapper for all
BatchJobs backends, which can be specified via the `backend` argument,
e.g.
```r
> plan(batchjobs, backend='.BatchJobs.R')
```

Below are the most common types of BatchJobs backends.  The default is
`backend="default"`.  That is, it is often enough to just use:
```r
> plan(batchjobs)
```


| Backend                | OSes        | Description                                                               | Alternative in future package
|:-----------------------|:------------|:--------------------------------------------------------------------------|:------------------------------------------------
| _synchronous:_         |             | _non-parallel:_                                                           |
| `interactive`          | all         | non-parallel processing in the current R process                          | `plan(eager)`
| `local`                | all         | non-parallel processing in a separate R process (on current machine)      | `plan(multisession, maxCores=2)`
| _asynchronous:_        |             | _parallel_:                                                               |
| `multicore`            | not Windows | forked R processes (on current machine) using all available cores         | `plan(multicore, maxCores=availableCores())`
| `multicore-1`          | not Windows | `multicore` using all but one of the available cores                      | `plan(multicore, maxCores=availableCores()-1L)`
| `multicore=n`          | not Windows | `multicore` using `n` the available cores                                 | `plan(multicore, maxCores=n)`
| _generic:_             |             |                                                                           |
| `/path/to.BatchJobs.R` | all         | According to specific `/path/to/.BatchJobs.R` configuration file          | N/A
| `.BatchJobs.R`         | all         | According to `./.BatchJobs.R` or `~/.BatchJobs.R`                         | N/A
| `default`              | all         | Same as `c(".BatchJobs.R", "multicore-1", "local")`.  This is the default | N/A

It is possible to specify a set of backend alternatives,
e.g. `plan(batchjobs, backend=c("multicore", "local"))`.  The first
available and supported backend on the running system will be used.
The fallback when any of the specified backends are not supported is
to use `backend="local"`.

It is also possible to define aliases for commonly used sets of
backends.  For instance,
```r
backend(cluster=c(".BatchJobs.R", "multicore", "local"))
```
creates backend alias `"cluster"` using whatever BatchJobs
configuration file is available with fallback to `"multicore"` and
`"local"`.  With this, we can use the above set of alternatives as:
```r
plan(batchjobs, backend="cluster")
```

### Example: A `.BatchJobs.R` file for TORQUE/PBS
The most powerful and most common usage of BatchJobs futures is via a
backend configured by a `.BatchJobs.R` file.  For example, to use
future that are distributed on a compute cluster via a TORQUE/PBS job
scheduler, use:
```r
library("future.BatchJobs")
plan(batchjobs)
```
and then use a `.BatchJobs.R` file (in the working directory or in
your home directory) with the following content:
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
For further details and examples on how to configure BatchJobs, see
the [BatchJobs configuration] wiki page.



## Demos
The [future] package provdides a demo using futures for calculating a
set of Mandelbrot planes.  Except from using futures, the demo does
not assume anything about what type of futures are used.  This is up
to the user to control.
For instance, to use `local` BatchJobs futures, run the demo as:
```r
library("future.BatchJobs")
plan(batchjobs, batchjobs="local")
demo("mandelbrot", package="future", ask=FALSE)
```


[BatchJobs]: http://cran.r-project.org/package=BatchJobs
[brew]: http://cran.r-project.org/package=brew
[future]: http://cran.r-project.org/package=future
[future.BatchJobs]: https://github.com/HenrikBengtsson/future.BatchJobs/
[BatchJobs configuration]: https://github.com/tudo-r/BatchJobs/wiki/Configuration

---
Copyright Henrik Bengtsson, 2015-2016

## Installation
R package future.BatchJobs is only available via [GitHub](https://github.com/HenrikBengtsson/future.BatchJobs) and can be installed in R as:
```r
source('http://callr.org/install#HenrikBengtsson/future.BatchJobs')
```

### Pre-release version
To install the pre-release version that is available in branch `develop`, use:
```r
source('http://callr.org/install#HenrikBengtsson/future.BatchJobs@develop')
```
This will install the package from source.  



## Software status

| Resource:     | GitHub        | Travis CI     | Appveyor         |
| ------------- | ------------------- | ------------- | ---------------- |
| _Platforms:_  | _Multiple_          | _Linux_       | _Windows_        |
| R CMD check   |  | <a href="https://travis-ci.org/HenrikBengtsson/future.BatchJobs"><img src="https://travis-ci.org/HenrikBengtsson/future.BatchJobs.svg" alt="Build status"></a> | <a href="https://ci.appveyor.com/project/HenrikBengtsson/future-batchjobs"><img src="https://ci.appveyor.com/api/projects/status/github/HenrikBengtsson/future.BatchJobs?svg=true" alt="Build status"></a> |
| Test coverage |                     | <a href="https://coveralls.io/r/HenrikBengtsson/future.BatchJobs"><img src="https://coveralls.io/repos/HenrikBengtsson/future.BatchJobs/badge.svg?branch=develop" alt="Coverage Status"/></a>   |                  |
