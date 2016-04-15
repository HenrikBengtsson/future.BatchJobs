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

### Global variables

Although the following expression is evaluated in an asynchronous
environment - separated from the calling one - the asynchronous
environment "inherits"(*) any "global" variables in the calling
environment and its parents.  For example,
```r
a <- 2
y %<-% { b <- a*3.14; b }
```
results in `y` being assigned `6.28`.

If a global variable is one that is assigned by another asynchronous
expression, then dependent asynchronous expressions will wait for the
former to complete in order to resolve the global variables.  For
example, in
```r
a %<-% { Sys.sleep(7); runif(1) }
b %<-% { Sys.sleep(2); rnorm(1) }
c %<-% { x <- a*b; Sys.sleep(2); abs(x) }
d <- runif(1)
```
the third asynchronous expression will not be evaluated until `a` and
`b` have taken their values.  As a matter of fact, even if `c` is also
an asynchronous assignment, R will pause (**) until global variables
`a` and `b` are resolved.  In other words, the assignment of `d` will
not take place until `a` and `b` are resolved (but there is no need to
wait for `c`).  This pause can be avoided using nested asynchronous
evaluation (see Section below).


_Footnotes_:
(\*) Since the asynchronous environment may be in a separate R session
on a physically different machine, the "inheritance" of "global"
variables is achieved by first identifying which variables in the
asynchronous expression are global and then copy them from the calling
environment to the asynchronous environment (using serialization).
This has to be taken into consideration when working with large
objects, which can take a substantial time to serialize.  Serialized
objects may also be written to file which then a compute node reads
back in. The global environments are identified using code inspection,
cf. the [codetools] package.
(\*\*) There is currently no lazy-evaluation mechanism for global
variables from asynchronous evaluations.  However, theoretically, one
could imagine that parts of an asynchronous expression can be
evaluated while the required one is still being evaluated.  However,
the current implementation is such that the asynchronous evaluation
will not be _initiated_ until all global variables can be resolved.


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



### Multi-core processing
Multi-core processing is when multiple R processes are used (instead
of the default single-thread single-process R session we are all used
to).  Note that multi-core processing is not available on Windows
(this is a limitation of the R core package `parallel`).
When specifying `plan(batchjobs, backend="multicore")`, all available
cores are used on the machine.  For heavy computations, this might
render the machine very slow and useless for other things.  To avoid
this, one can specify how many cores to "spare", e.g. `plan(batchjobs,
backend="multicore-2")` will use all but two cores. Note how the
default (see above) is `plan(batchjobs, backend="multicore-1")`. As an
alternative, it is also possible to specify the exact number of cores
to be used, e.g. `plan(batchjobs, backend="multicore=3")`.


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


## Future directions
The above, which is a fully functional prototype, relies 100% on
[BatchJobs] as the backend.  Unfortunately, BatchJobs has some
limitations as it stands.  The most important one is the fact that [all
machines, including the head machine, have to share the same file
system](https://github.com/tudo-r/BatchJobs/issues/71).  This means
that it is, for instance, not possible to do asynchronous evaluation
on remote hosts, e.g. over ssh.  If that would be possible, then one
can imagine doing things such as:
```r
# The world's computer resources at your R prompt
library(async)
plan(batchjobs)

tasks %<-% {
  update.packages()
} %backends% c("local", "cluster", "AmazonEC2", "GoogleCompEngine")

tcga %<-% {
  plan(batchjobs, backend="cluster")

  a %<-% {
    doCRMAv2("BreastCancer", chipType="GenomeWideSNP_6")
  }

  b %<-% {
    doCRMAv2("ProstateCancer", chipType="Mapping250K_Nsp")
  }

  list(a=a, b=b)
} %plan% batchjobs(backend="AmazonEC2")

hapmap %<-% {
  plan(batchjobs, backend="cluster")

  normals %<-% {
    doCRMAv2("HapMap2", chipType="GenomeWideSNP_6")
  }

  normals
} %plan% batchjobs(backend="GoogleCompEngine")

```
Obviously great care needs to be taken in order to minimize the amount
of data sent back and forth, e.g. returning really large objects.

In order for the above to work, one would have to extend the
BatchJobs Registry framework to work across file systems, which
would requiring serialization and communicating over sockets.
A better approach is probably to instead use the [BiocParallel]
package as the main framework for backends.  BiocParallel "aims to
provide a unified interface to existing parallel infrastructure where
code can be easily executed in different environments".  It already
has built in support for BatchJobs.  More importantly, it support many
other backends, including Simple Network of Workstations ("SNOW")
style clusters. A SNOW cluster consists of a set of local or remote
workers that communicates with the head node/machine via sockets such
that data and commands can be transferred across using a serialized
protocol.  For example, from the BiocParallel vignette:
```r
hosts <- c("rhino01", "rhino01", "rhino02")
param <- SnowParam(workers = hosts, type = "PSOCK")
Execute FUN 4 times across the workers.
> FUN <- function(i) system("hostname", intern=TRUE)
> bplapply(1:4, FUN, BPPARAM = param)
```

Because of this, the next plan is to update 'async' to work on top of
BiocParallel instead.


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
set of known machines (for which you have ssh access and that
[share the same file system as your head
machine](https://github.com/tudo-r/BatchJobs/issues/71)), let the
`.BatchJobs.R` file contain:
```r
cluster.functions <- makeClusterFunctionsSSH(
  makeSSHWorker(nodename="n6", max.jobs=2),
  makeSSHWorker(nodename="n8"),
  makeSSHWorker(nodename="n12")
)
```

To use a more formal cluster system with a Torque/PBS job queue
mechanism, use:
```r
cluster.functions <- local({
  tmpl <- system.file(package="async", "config", "pbs.tmpl")
  makeClusterFunctionsTorque(tmpl)
})
```
where the "tmpl" file is a [brew]-embedded PBS script template.

For further details and examples on how to configure BatchJobs,
see the [BatchJobs configuration] wiki page.


[BatchJobs]: http://cran.r-project.org/package=BatchJobs
[BiocParallel]: http://bioconductor.org/packages/release/bioc/html/BiocParallel.html
[brew]: http://cran.r-project.org/package=brew
[codetools]: http://cran.r-project.org/package=codetools
[globals]: http://cran.r-project.org/package=globals
[future]: http://cran.r-project.org/package=future
[listenv]: http://cran.r-project.org/package=listenv
[async]: https://github.com/HenrikBengtsson/async/
[BatchJobs configuration]: https://github.com/tudo-r/BatchJobs/wiki/Configuration

---
Copyright Henrik Bengtsson, 2015-2016
