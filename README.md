# R package async - Asynchronous evaluation

Copyright Henrik Bengtsson, 2015

## Asynchronous evaluation
_Asynchronous evaluation_ is a method for evaluating multiple R
expressions in, typically, a parallel or distributed fashion such that
the "observed" total time for computing all values is less that if
the expressions would be evaluated synchronously (sequentially).

For instance, the following evaluation, which is synchronous, takes about 10 seconds to complete:

```r
> x <- { Sys.sleep(5); 3.14 }
> y <- { Sys.sleep(5); 2.71 }
> z <- x + y
[1] 5.85
```

whereas the following _asynchronous_ evaluation only takes
about 5 seconds to complete when done on a
multi-core system:

```r
> library('async')
> x %<=% { Sys.sleep(5); 3.14 }
> y %<=% { Sys.sleep(5); 2.71 }
> z <- x + y
[1] 5.85
```


### Evaluation is done in a "local" environment
Each _asynchronous expression_ is evaluated in its own unique _asynchronous environment_, which is different from the calling environment.  The only way to transfer information from the asynchronous environment to the calling environment is via the (return) value, just as when functions are called and their values are returned.   In other words,

```r
x %<=% { a <- 3.14 }
```

is effectively equivalent to

```r
x %<=% local({ a <- 3.14 })
```

I both cases _asynchronous variable_ 'a' with be assigned value `3.14` in a "local" environment.  Since this is the last value in the expression, it is also the value of the asynchronous expression, which is therefore also the value "returned" (in R there is no need to "return" values; it is always the last value of the expression that will be used).  This is the value that will be assigned to variable `x` in the calling environment.  Asynchronous variable `a` is gone forever.  As a matter of fact, it is _not_ possible for an asynchronous expression to assign variables in the calling environment, i.e. assignments such as `<-`, `<<-` and `assign()` only affects the asynchronous environment.


### Global variables

Although the following expression is evaluated in an asynchronous environment - separated from the calling one - the asynchronous environment "inherits"(*) any "global" variables in the calling environment and its parents.  For example,
```r
a <- 2
y %<=% { b <- a*3.14; b }
```
results in `y` being assigned `6.28`.

If a global variable is one that is assigned by another asynchronous expression, then dependent asynchronous expressions will wait for the former to complete in order to resolve the global variables.  For example, in
```r
a %<=% { Sys.sleep(7); runif(1) }
b %<=% { Sys.sleep(2); rnorm(1) }
c %<=% { x <- a*b; Sys.sleep(2); abs(x) }
d <- runif(1)
```
the third asynchronous expression will not be evaluated until `a` and `b` have taken their values.  As a matter of fact, even if `c` is also an asynchronous assignment, R will pause (**) until global variables `a` and `b` are resolved.  In other words, the assignment of `d` will not take place until `a` and `b` are resolved (but there is no need to wait for `c`).  This pause can be avoided using nested asynchronous evaluation (see Section below).


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


## Nested asynchronous evaluation
It is possible to nest multiple levels of asynchronous evaluations, e.g.
```r
c %<=% {
  a %<=% { Sys.sleep(7); runif(1) }
  b %<=% { Sys.sleep(2); rnorm(1) }
  x <- a*b; Sys.sleep(2); abs(x)
}
d <- runif(1)
```
This will evaluate the expression for `c` asynchronously such that `d` is assigned almost momentarily.  In turn, the value for `c` will be resolved when _nested asynchronous expressions_ for local variables `a` and `b` have been evaluated.


## Other types of asynchronous assignments
The `%<=%` assignment operator _cannot_ be used in all cases where regular `<-` assignment operator can be used.  This is because `%<=%` assignments are _delayed assignment_, cf. `help("delayedAssign")`.  As shown above, `%<=%` can be used for assignment of (asynchronous) values to variables (formally symbols).  It can also be use to assign to variables in _environments_.  For example,
```r
> env <- new.env()
> env$a %<=% { 1 }
> env[["b"]] %<=% { 2 }
> name <- "c"
> env[[name]] %<=% { 3 }
```

The limitations of delayed asynchronous assignments are the same as the limitations that `assign()` has, i.e. you can assign to variables and you can specify the target environment.  This means that you, for instance, cannot assign to an element of a vector, matrix, list or a data.frame.  If tried, an informative error will be generated, e.g.
```r
> x <- list()
> x$a %<=% { 1 }
Error: Delayed assignments can not be done to a 'list'; only to a variable and a
n environment: x$a
```

**(TO DO: Indexed environments are not yet implemented)**

If _indexed subsetting_ is needed, one can instead use an
_indexed environment_, e.g. 
```r
x <- idxenv()
for ii in 1:3) {
  x[[ii]] %<=% { rnorm(ii) }
}
names(x) <- c("a", "b", "c")
```
The asynchroneous values can retrieved individually as `x[[2]]`,
`x[["b"]]` and `x$b`.  All values can be retrieved as a list as
`as.list(x)`.  As with asynchroneous values, retrieving one or more of
them from and indexed environment will cause R to pause until all
requested values are available, that is, until all corresponding
asynchroneous evaluations have been completed.



## Exception handling
If an error occurs during the evaluation of an asynchronous
expression, that error is thrown when the asynchronous value is
retrieved.  For example:
```r
> e %<=% { stop("Woops!") }
> 1+2
[1] 3
> e
Error: BatchJobError: 'Error in eval(expr, envir = envir) : Woops! '
```
This error is rethrown each time `e` is retrieved, so it is not
possible to "inspect" `e` any further using standard R functions such
as `print()` and `str()`.
In order to troubleshoot an error, one can use the `inspect()` function
to retrieve the underlying asynchroneous "task" object, e.g.
```r
> inspect(e)
AsyncTask:
Expression:
  {
      stop("Woops!")
  }
Status: 'error', 'started', 'submitted'
Error: 'Error in eval(expr, envir = envir) : Woops! '
Backend:
Job registry:  async1189072551
  Number of jobs:  1
  Files dir: T:/async1189072551-files
  Work dir: T:/
  Multiple result files: FALSE
  Seed: 544948890
  Required packages: BatchJobs
```


## Choosing backend
The asynchronous evaluation done by the [async] package uses the [BatchJobs] package as a backend for effectuating the computations.  This can be configured using the `backend()` function.  Examples:

* `backend("default")` - use `.BatchJobs.R` configuration file,
   if available. If not, use `"multicore-1"` if supported,
   otherwise use `"interactive"`
* `backend("multicore")` - parallel processing using all available
   cores on the local machine.
* `backend("multicore-1")` - parallel processing using all but one
   of the available cores on the local machine.
* `backend("local")` - non-parallel processing in a separate R process.
* `backend("interactive")` - non-parallel processing in the
   current R session.
* `backend(".BatchJobs.R")` - use `.BatchJobs.R` configuration file.

It is possible to specify a set of possible backends,
e.g. `backend(c("multicore", "interactive"))`.  The first
available/supported backend will be used.

If none of the requested backends work/are supported, the fallback is
always to use the `"interactive"` which is available on all systems.

To see what the most recently set backend was, use `backend(NULL)`.
To reset, use `backend("reset")`
(which is equivalent to `backend("default")`).



### Multi-core processing
Multi-core processing is when multiple R processes are used (instead of the
default single-thread single-process R session we are all used to).
Note that multi-core processing is not available on Windows (this is a
limitation of the R core package `parallel`).
When specifying `backend("multicore")`, all available cores are used on the
machine.  For heavy computations, this might render the machine very slow and
useless for other things.  To avoid this, one can specify how many cores to
"spare", e.g. `backend("multicore-2")` will use all but two cores.
Note how the default (see above) is `backend("multicore-1")`.
As an alternative, it is also possible to specify the exact number of cores
to be used, e.g. `backend("multicore=3")`.


### Advanced configuration
For more complicated backends (e.g. clusters), one has to use
BatchJobs-specific configuration files as explained in the Appendix.
The default is to use such configuration files, if available.  To
explicitly use such backend configurations, use `backend(".BatchJobs.R")`.


### Backend aliases
It is possible to create aliases for favorite sets of backends.  For instance,
```r
backend(cluster=c(".BatchJobs.R", "multicore", "local"))
```
creates backend alias `"cluster"` using whatever BatchJobs
configuration file is available with fallbacks to `"multicore"`
and `"local"`.  After setting an alias it can be specified as:
```r
backend("cluster")
```


### Evaluate asynchronous expression on specific backend
Asynchronous expressions are processed by the default backend as given by `backend(NULL)`.  If another backend should be used to evaluate for a particular expression, operator `%backend%` can be used.  For example,
```r
a %<=% { Sys.sleep(7); runif(1) } %backend% "multicore-2"
b %<=% { Sys.sleep(2); rnorm(1) } %backend% "cluster"
c %<=% { x <- a*b; Sys.sleep(2); abs(x) }
d <- runif(1)
```
In this case expression `a` will be processed by the `multicore-2` backend, expression `c` by the `cluster` backend, and expression `c` by the default backend.

Backend specifications can also be used in nested asynchronous evaluations:
```r
backend("cluster")
a %<=% { Sys.sleep(7); runif(1) }
c %<=% {
  b %<=% { Sys.sleep(2); rnorm(1) } %backend% "multicore=2"
  x <- a*b; Sys.sleep(2); abs(x)
}
d <- runif(1)
```

## Examples

### Download files in parallel
```r
library('async')
library('R.utils')
repos <- c(CRAN="http://cran.r-project.org",
           Bioc="http://www.bioconductor.org/packages/release/bioc")
urls <- sapply(repos, file.path, "src/contrib/PACKAGES", fsep="/")
files <- new.env()
for (name in names(urls)) {
  files[[name]] %<=% downloadFile(urls[[name]], path=name)
}
str(as.list(files))
```


## Availability
This package is only available via GitHub.  Install in R as:

```s
source('http://callr.org/install#HenrikBengtsson/async')
```


## Future directions
If it would be possible to abstract the [BatchJobs] Registry layer
even further such that the job registry can be distributed on
disconnected file systems and synchronized via serialization, say,
over ssh, then one can image to do things such as:
```r
# The world at your R prompt
tcga %<=% {
  backend("cluster")

  a %<=% {
    doCRMAv2("BreastCancer", chipType="GenomeWideSNP_6")
  }

  b %<=% {
    doCRMAv2("ProstateCancer", chipType="Mapping250K_Nsp")
  }

  list(a=a, b=b)
} %backend% "Amazon Web Services"

hapmap %<=% {
  backend("cluster")

  normals %<=% {
    doCRMAv2("HapMap2", chipType="GenomeWideSNP_6")
  }

  normals
} %backend% "Google Compute Engine"


task %<=% {
  backup_machine()
} %backend% "@home"
```
Obviously great care needs to be taken in order to minimize the amount
of data sent back and forth, e.g. returning really large objects.
  

## Appendix

### Configuration of backend for parallel / distributed processing
Basic backends can be configured using the `backend()` function.
For full control, or for more complicated backends such as clusters,
one can use the configuration options available from the BatchJobs
package.  In summary, this type of configuration is done via a
`.BatchJobs.R` configuration file that can reside in either the
current directory or the user's home directory
(this file is only needed on compute nodes if nested asynchronous
calls should also use the same configuration).  These settings
are used by default if available.  They also be explicitly specified
by `backend(".BatchJobs.R")`.

For example, to configure BatchJobs to distribute computations on a set of known machines (for which you have ssh access), let the `.BatchJobs.R` file contain:
```r
cluster.functions <- makeClusterFunctionsSSH(
  makeSSHWorker(nodename="n6", max.jobs=2),
  makeSSHWorker(nodename="n8"),
  makeSSHWorker(nodename="n12")
)
```

To use a more formal cluster system with a Torque/PBS job queue mechanism, use:
```r
cluster.functions <- local({
  tmpl <- system.file(package="async", "config", "pbs.tmpl")
  makeClusterFunctionsTorque(tmpl)
})
```
where the "tmpl" file is a [brew]-embedded PBS script template.

For further details and examples on how to configure BatchJobs,
see the [BatchJobs configuration] wiki page.


## Indexed environments
The async package provides _indexed environments_, which is a class of
environments that emulates part of what can be done with lists,
specifically they supports _subsetting by indices_.  For example,
```r
> x <- idxenv()
> x[[1]] %<=% { 1 }
> x[[3]] %<=% { "Hello world!" }
> length(x)
3
> seq_along(x)
[1] 1 2 3
> names(x) <- c("a", "b", "c")
> x$b <- TRUE
> x[[1]]
1
> as.list(x)
$a
[1] 1

$b
[1] TRUE

$c
[1] "Hello world!"
```

It is possible to also specify the length upfront, e.g.
```r
> x <- idxenv(length=4)
> seq_along(x)
[1] 1 2 3 4
```


### Software quality

* R CMD check status: <a
  href="https://travis-ci.org/HenrikBengtsson/async"><img
  src="https://travis-ci.org/HenrikBengtsson/async.svg?branch=master"
  alt="Build status"></a>
* Test coverage status: <a
  href='https://coveralls.io/r/HenrikBengtsson/async?branch=develop'><img
  src='https://coveralls.io/repos/HenrikBengtsson/async/badge.png?branch=develop'
  alt='Coverage Status' /></a>


[async]: https://github.com/UCSF-CBC/async/
[brew]: http://cran.r-project.org/package=brew
[BatchJobs]: http://cran.r-project.org/package=BatchJobs
[BatchJobs configuration]: https://github.com/tudo-r/BatchJobs/wiki/Configuration
[codetools]: cran.r-project.org/package=codetools
