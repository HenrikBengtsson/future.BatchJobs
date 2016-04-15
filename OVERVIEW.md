## Introduction
This package provides [BatchJobs] futures on the Future API
defined by the [future] package.  This means that all of the
BatchJobs machinery can be utilized using futures, e.g.
```r
> library('async')
> plan(batchjobs, backend='.BatchJobs.R')
>
> x %<-% { Sys.sleep(5); 3.14 }
> y %<-% { Sys.sleep(5); 2.71 }
> x + y
[1] 5.85
```
For an introduction how to use futures in R, please consult the
vignettes of the [future] package.


## Choosing BatchJobs backend
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
#PBS -N <%%= job.name %%>
## merge standard error and output
#PBS -j oe
## direct streams to our logfile
#PBS -o <%%= log.file %%>
#PBS -l walltime=<%%= resources$walltime %%>,nodes=<%%= resources$nodes %%>,vmem=<%%= resources$memory %%>M
## remove this line if your cluster does not support arrayjobs
#PBS -t 1-<%%= arrayjobs %%>
  
## Run R:
## we merge R output with stdout from PBS, which gets then logged via -o option
R CMD BATCH --no-save --no-restore "<%%= rscript %%>" /dev/stdout
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
