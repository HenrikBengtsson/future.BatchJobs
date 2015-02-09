# R package async - Asynchroneous evaluation

Copyright Henrik Bengtsson, 2015

## Asynchroneous evaluation
_Asynchroneous evaluation_ is a method for evaluting multiple R
expressions in, typically, a parallel or distributed fashion such that
the "observed" total time for computing all values is less that if
the expressions would be evaluated synchroneously (sequentially).

For instance, the following evaluation, which is synchroneous, takes approximately 10 seconds to complete:

```r
> x <- { Sys.sleep(5); 3.14 }
> y <- { Sys.sleep(5); 2.71 }
> z <- x + y
[1] 5.85
```

whereas the follwing _asynchroneous_ evaluation only takes
approximately 5 seconds to complete when done on a 
multi-core system:

```r
> library(async)
> x %<=% { Sys.sleep(5); 3.14 }
> y %<=% { Sys.sleep(5); 2.71 }
> z <- x + y
[1] 5.85
```


## Evaluation is done in a "local" environment
Each _asynchroneous expression_ is evaluated in its own unique _asynchroneous environment_, which is different from the calling environment.  The only way to transfer information from the asynchroneous environment to the calling environment, is via the (return) value, just as when functions are called and their values are returned.   In other words,

```r
x %<=% { a <- 3.14 }
```

is effectively equivalent to

```r
x %<=% local({ a <- 3.14 })
```

I both cases _asynchroneous variable_ 'a' with be assigned value `3.14` in a "local" environment.  Since this is the last value in the expression, it is also the value of the asynchroneous expression, so there is no need to explicitly "return" `a`.  This is the value that will be assigned to variable `x` in the calling environment.  This is also how function calls work.

As a matter of fact, it is _not_ possible to for an asynchroneous expression to assign variable in the calling environment, i.e. assignments such as `<-`, `<<-` and `assign()` only affects the asynchrenous environment.


### Global variables

Although the following expression is evaluated in an asynchroneous environment, which is separate from the calling one, the asynchroneous environment "inherits"(*) all "global" variables in the calling environment and its parents.  For example,
```r
a <- 2
y %<=% { b <- a*3.14; b }
```
will result in `y` being assigned `6.28`.

_Footnote_: (*) Details: Since the asynchroneous environment may be in a separate R session on a physically different machine, the "inheritance" of "global" variables is achieved by first identifying which variables in the asynchroneous expression are global and then copy them from the calling environment to the asynchroneous environment (using serialization).  This has to be taken into consideration when working with large objects, which can take a substational time to serialize (and often write to file which then a compute node reads back). The global environments are identified using code inspection, cf. the [codetools] package.


## Availability
This package is only available via GitHub.  Install in R as:

```s
source('http://callr.org/install#HenrikBengtsson/async')
```

## Appendix

### Configuration of backend for parallel / distributed processing
The asynchroneous evaluation done by the [async] package uses the
[BatchJobs] package as a backend for effectuating the computations.
The default BatchJobs setup is to evaluate all expression in the
current R session.

In order to perform parallel computations, a `.BatchJobs.R` 
configuration file is required, which can reside either in
the current directory or the user's home directory
(this file is _not_ needed on compute nodes).

Below are some examples of `.BatchJobs.R` configuration scripts.

#### Interactive non-parallel processing (default)
```r
cluster.functions <- makeClusterFunctionsInteractive()
```

#### Non-interactive non-parallel processing (via Rscript)
```r
cluster.functions <- async::makeClusterFunctionsLocal()
```

#### Parallel multi-core processing (on local machine)
```r
cluster.functions <- makeClusterFunctionsMulticore(
  ncpus=parallel::detectCores()
)
```

#### Distributed processing (on known machines)
```r
cluster.functions <- makeClusterFunctionsSSH(
  makeSSHWorker(nodename="n6", max.jobs=2),
  makeSSHWorker(nodename="n8"),
  makeSSHWorker(nodename="n12")
)
```

#### Distributed processing (on a Torque/PBS cluster)
```r
cluster.functions <- local({
  file <- system.file(package="async", "config", "pbs.tmpl")
  makeClusterFunctionsTorque(file)
})
```


For further details and examples on how to configure BatchJobs,
see the [BatchJobs configuration] wiki page.


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
[BatchJobs]: http://cran.r-project.org/package=BatchJobs
[BatchJobs configuration]: https://github.com/tudo-r/BatchJobs/wiki/Configuration
[codetools]: cran.r-project.org/package=codetools
