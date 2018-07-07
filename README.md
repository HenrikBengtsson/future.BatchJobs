# future.BatchJobs: A Future API for Parallel and Distributed Processing using BatchJobs

_NOTE: The [BatchJobs](https://cran.r-project.org/package=BatchJobs) package is deprecated in favor of the [batchtools](https://cran.r-project.org/package=batchtools) package. Because of this, it is recommended to use the [future.batchtools](https://cran.r-project.org/package=future.batchtools) package instead of this package. This package will only be updated in order for it to be compliant with updates in R and the [future](https://cran.r-project.org/package=future) package._

## Introduction
The [future] package provides a generic API for using futures in R.
A future is a simple yet powerful mechanism to evaluate an R expression
and retrieve its value at some point in time.  Futures can be resolved
in many different ways depending on which strategy is used.
There are various types of synchronous and asynchronous futures to
choose from in the [future] package.

This package, [future.BatchJobs], provides a type of futures that
utilizes the [BatchJobs] package.  This means that _any_ type of
backend that the BatchJobs package supports can be used as a future.
More specifically, future.BatchJobs will allow you or users of your
package to leverage the compute power of high-performance computing
(HPC) clusters via a simple switch in settings - without having to
change any code at all.

For instance, if BatchJobs is properly configures, the below two
expressions for futures `x` and `y` will be processed on two different
compute nodes:
```r
> library("future.BatchJobs")
> plan(batchjobs_custom)
>
> x %<-% { Sys.sleep(5); 3.14 }
> y %<-% { Sys.sleep(5); 2.71 }
> x + y
[1] 5.85
```
This is obviously a toy example to illustrate what futures look like
and how to work with them.

A more realistic example comes from the field of cancer research
where very large data FASTQ files, which hold a large number of short
DNA sequence reads, are produced.  The first step toward a biological
interpretation of these data is to align the reads in each sample
(one FASTQ file) toward the human genome.  In order to speed this up,
we can have each file be processed by a separate compute node and each
node we can use 24 parallel processes such that each process aligns a
separate chromosome.  Here is an outline of how this nested parallelism
could be implemented using futures.
```r
library("future")
library("listenv")
## The first level of futures should be submitted to the
## cluster using BatchJobs.  The second level of futures
## should be using multiprocessing, where the number of
## parallel processes is automatically decided based on
## what the cluster grants to each compute node.
plan(list(batchjobs_custom, multiprocess))

## Find all samples (one FASTQ file per sample)
fqs <- dir(pattern="[.]fastq$")

## The aligned results are stored in BAM files
bams <- listenv()

## For all samples (FASTQ files) ...
for (ss in seq_along(fqs)) {
  fq <- fqs[ss]

  ## ... use futures to align them ...
  bams[[ss]] %<-% {
    bams_ss <- listenv()
	## ... and for each FASTQ file use a second layer
	## of futures to align the individual chromosomes
    for (cc in 1:24) {
      bams_ss[[cc]] %<-% htseq::align(fq, chr=cc)
    }
	## Resolve the "chromosome" futures and return as a list
    as.list(bams_ss)
  }
}
## Resolve the "sample" futures and return as a list
bams <- as.list(bams)
```
Note that a user who do not have access to a cluster could use the same script processing samples sequentially and chromosomes in parallel on a single machine using:
```r
plan(list(sequential, multiprocess))
```
or samples in parallel and chromosomes sequentially using:
```r
plan(list(multiprocess, sequential))
```

For an introduction as well as full details on how to use futures,
please consult the package vignettes of the [future] package.



## Choosing BatchJobs backend
The future.BatchJobs package implements a generic future wrapper
for all BatchJobs backends.  Below are the most common types of
BatchJobs backends.


| Backend                 | Description                                                              | Alternative in future package
|:------------------------|:-------------------------------------------------------------------------|:------------------------------------
| _generic:_              |                                                                          |
| `batchjobs_custom`      | Uses custom BatchJobs configuration script files, e.g. `.BatchJobs.R`    | N/A
| _predefined:_           |                                                                          |
| `batchjobs_torque`      | Futures are evaluated via a [TORQUE] / PBS job scheduler                 | N/A
| `batchjobs_slurm`       | Futures are evaluated via a [Slurm] job scheduler                        | N/A
| `batchjobs_sge`         | Futures are evaluated via a [Sun/Oracle Grid Engine (SGE)] job scheduler | N/A
| `batchjobs_lsf`         | Futures are evaluated via a [Load Sharing Facility (LSF)] job scheduler  | N/A
| `batchjobs_openlava`    | Futures are evaluated via an [OpenLava] job scheduler                    | N/A
| `batchjobs_interactive` | synchronous evaluation in the calling R environment                      | `plan(transparent)`
| `batchjobs_local`       | synchronous evaluation in a separate R process (on current machine)      | `plan(cluster, workers="localhost")`

In addition to the above, there is also `batchjobs_multicore` (which on Windows and Solaris falls back to `batchjobs_local`), which runs BatchJobs tasks asynchronously in background R sessions (sic!) on the current machine.  We _advise to not use_ this and instead use `multisession` of the [future] package.  For details, see `help("batchjobs_multicore")`.


### Examples

Below are two examples illustrating how to use `batchjobs_custom` and `batchjobs_torque` to configure the BatchJobs backend.  For further details and examples on how to configure BatchJobs, see the [BatchJobs configuration] wiki page.

### Example: A .BatchJobs.R file using local BatchJobs
The most general way of configuring BatchJobs is via a `.BatchJobs.R` file.
This file should be located in the current directory or in the user's
home directory.  For example, as an alternative to `batchjobs_local`,
we can manually configure local BatchJobs futures a `.BatchJobs.R` file
that contains
```r
cluster.functions <- makeClusterFunctionsLocal()
```
This will then be found and used when specifying
```r
> plan(batchjobs_custom)
```
To specify this BatchJobs configuration file explicitly, one can use
```r
> plan(batchjobs_custom, pathname="./.BatchJobs.R")
```

This follows the naming convention set up by the BatchJobs package.



### Example: A .BatchJobs.*.tmpl template file for TORQUE / PBS
To configure BatchJobs for job schedulers we need to setup a template
file that is used to generate the script used by the scheduler.
This is what a template file for TORQUE / PBS may look like:
```sh
## Job name:
#PBS -N <%= job.name %>

## Merge standard error and output:
#PBS -j oe

## Resource parameters:
<% for (name in names(resources)) { %>
#PBS -l <%= name %>=<%= resources[[name]] %>
<% } %>

## Run R:
R CMD BATCH --no-save --no-restore "<%= rscript %>" /dev/stdout
```
If this template is saved to file `.BatchJobs.torque.tmpl` in the
working directory or the user's home directory, then it will be
automatically located and loaded when doing:
```r
> plan(batchjobs_torque)
```
Resource parameters can be specified via argument `resources` which should be a named list and is passed as is to the template file.  For example, to request that each job would get alloted 12 cores (one a single machine) and up to  5 GiB of memory, use:
```r
> plan(batchjobs_torque, resources=list(nodes="1:ppn=12", vmem="5gb"))
```

To specify the `resources` argument at the same time as using nested future strategies, one can use `tweak()` to tweak the default arguments.  For instance,
```r
plan(list(
  tweak(batchjobs_torque, resources=list(nodes="1:ppn=12", vmem="5gb")),
  multiprocess
))
```
causes the first level of futures to be submitted via the TORQUE job scheduler requesting 12 cores and 5 GiB of memory per job.  The second level of futures will be evaluated using multiprocessing using the 12 cores given to each job by the scheduler.

A similar filename format is used for the other types of job schedulers supported.  For instance, for Slurm the template file should be named `.BatchJobs.slurm.tmpl` in order for
```r
> plan(batchjobs_slurm)
```
to locate the file automatically.  To specify this template file explicitly, use argument `pathname`, e.g.
```r
> plan(batchjobs_slurm, pathname="./.BatchJobs.slurm.tmpl")
```


Note that it is still possible to use a `.BatchJobs.R` and load the template file using a standard BatchJobs approach for maximum control.  For further details and examples on how to configure BatchJobs per se, see the [BatchJobs configuration] wiki page.



## Demos
The [future] package provides a demo using futures for calculating a
set of Mandelbrot planes.  The demo does not assume anything about
what type of futures are used.
_The user has full control of how futures are evaluated_.
For instance, to use local BatchJobs futures, run the demo as:
```r
library("future.BatchJobs")
plan(batchjobs_local)
demo("mandelbrot", package="future", ask=FALSE)
```


[BatchJobs]: https://cran.r-project.org/package=BatchJobs
[brew]: https://cran.r-project.org/package=brew
[future]: https://cran.r-project.org/package=future
[future.BatchJobs]: https://cran.r-project.org/package=future.BatchJobs
[BatchJobs configuration]: https://github.com/tudo-r/BatchJobs/wiki/Configuration
[TORQUE]: https://en.wikipedia.org/wiki/TORQUE
[Slurm]: https://en.wikipedia.org/wiki/Slurm_Workload_Manager
[Sun/Oracle Grid Engine (SGE)]: https://en.wikipedia.org/wiki/Oracle_Grid_Engine
[Load Sharing Facility (LSF)]: https://en.wikipedia.org/wiki/Platform_LSF
[OpenLava]: https://en.wikipedia.org/wiki/OpenLava


## Installation
R package future.BatchJobs is available on [CRAN](https://cran.r-project.org/package=future.BatchJobs) and can be installed in R as:
```r
install.packages('future.BatchJobs')
```

### Pre-release version

To install the pre-release version that is available in Git branch `develop` on GitHub, use:
```r
source('http://callr.org/install#HenrikBengtsson/future.BatchJobs@develop')
```
This will install the package from source.  



## Contributions

This repository uses the [Git Flow](http://nvie.com/posts/a-successful-git-branching-model/) branching model (the [`git flow`](https://github.com/petervanderdoes/gitflow-avh) extension is useful for this).  The [`develop`](https://github.com/HenrikBengtsson/future.BatchJobs/tree/develop) branch contains the latest contributions and other code that will appear in the next release, and the [`master`](https://github.com/HenrikBengtsson/future.BatchJobs) branch contains the code of the latest release, which is exactly what is currently on CRAN (see below).

Contributing to this package is easy.  Just send a [pull request](https://help.github.com/articles/using-pull-requests/).  When you send your PR, make sure `develop` is the destination branch on the [future.BatchJobs repository](https://github.com/HenrikBengtsson/future.BatchJobs).  Your PR should pass `R CMD check --as-cran`, which will also be checked by <a href="https://travis-ci.org/HenrikBengtsson/future.BatchJobs">Travis CI</a> and <a href="https://ci.appveyor.com/project/HenrikBengtsson/future-batchjobs">AppVeyor CI</a> when the PR is submitted.


## Software status

| Resource:     | CRAN        | Travis CI       | Appveyor         |
| ------------- | ------------------- | --------------- | ---------------- |
| _Platforms:_  | _Multiple_          | _Linux & macOS_ | _Windows_        |
| R CMD check   | <a href="https://cran.r-project.org/web/checks/check_results_future.BatchJobs.html"><img border="0" src="http://www.r-pkg.org/badges/version/future.BatchJobs" alt="CRAN version"></a> | <a href="https://travis-ci.org/HenrikBengtsson/future.BatchJobs"><img src="https://travis-ci.org/HenrikBengtsson/future.BatchJobs.svg" alt="Build status"></a>   | <a href="https://ci.appveyor.com/project/HenrikBengtsson/future-batchjobs"><img src="https://ci.appveyor.com/api/projects/status/github/HenrikBengtsson/future.BatchJobs?svg=true" alt="Build status"></a> |
| Test coverage |                     | <a href="https://codecov.io/gh/HenrikBengtsson/future.BatchJobs"><img src="https://codecov.io/gh/HenrikBengtsson/future.BatchJobs/branch/develop/graph/badge.svg" alt="Coverage Status"/></a>     |                  |
