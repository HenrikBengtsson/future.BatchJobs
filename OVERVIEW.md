## Introduction
The [async] package provides [BatchJobs] futures according to
the Future API defined by the [future] package.
This means that all of the BatchJobs machinery can be
utilized using futures, e.g.
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
The async package implements a generic future wrapper for all
BatchJobs backends, which can be specified via the `backend` argument,
e.g.
```r
> plan(batchjobs, backend='.BatchJobs.R')
```

Below are the most common types of BatchJobs backends.  The default is `backend="default"`.  

| Backend                   | OSes        | Description       | Alternative in future package
|:--------------------------|:------------|:------------------|:-------------------------------
| _synchronous:_            |             | _non-parallel:_   |
| `interactive`             | all         | non-parallel processing in the current R process.  | plan(eager)
| `local`                   | all         | non-parallel processing in a separate R process.   | plan(multisession, maxCores=2)
| _asynchronous:_           |             | _parallel_:       |
| `multicore`               | not Windows | forked R processes (on current machine) using all available cores   | plan(multicore, maxCores=availableCores())
| `multicore-1`             | not Windows | forked R processes (on current machine) using all but one of the available cores   | plan(multicore, maxCores=availableCores()-1L)
| `multicore=<n>`             | not Windows | forked R processes (on current machine) using `<n>` the available cores   | plan(multicore, maxCores=<n>)
| _generic:_                |             |                   |
| `/path/to.BatchJobs.R`    | all         | According to specific `/path/to/.BatchJobs.R` configuration file.  | N/A
| `.BatchJobs.R`            | all         | According to `./.BatchJobs.R` or `~/.BatchJobs.R`.  | N/A
| `default`                 | all         | Same as `c(".BatchJobs.R", "multicore-1", "local")`.  This is the default.  | N/A

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


## Demos
The [future] package provdides a demo using futures for calculating a
set of Mandelbrot planes.  Except from using futures, the demo does
not assume anything about what type of futures are used.  This is up
to the user to control.

For instance, to use `local` BatchJobs futures, run the demo as:
```r
library("async")
plan(batchjobs, batchjobs="local")
demo("mandelbrot", package="future", ask=FALSE)
```


[BatchJobs]: http://cran.r-project.org/package=BatchJobs
[brew]: http://cran.r-project.org/package=brew
[future]: http://cran.r-project.org/package=future
[async]: https://github.com/HenrikBengtsson/async/
[BatchJobs configuration]: https://github.com/tudo-r/BatchJobs/wiki/Configuration

---
Copyright Henrik Bengtsson, 2015-2016
