# future.BatchJobs: A Future for BatchJobs

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
## what the cluster allots to each compute node.
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
plan(list(eager, multiprocess))
```
or samples in parallel and chromosomes sequentially using:
```r
plan(list(multiprocess, eager))
```

For an introduction as well as full details on how to use futures,
please consult the package vignettes of the [future] package.



## Choosing BatchJobs backend
The future.BatchJobs package implements a generic future wrapper
for all BatchJobs backends.  Below are the most common types of
BatchJobs backends.


| Backend                | OSes        | Description                                                               | Alternative in future package
|:-----------------------|:------------|:--------------------------------------------------------------------------|:------------------------------------
| _generic:_             |             |                                                                           |
| `batchjobs_custom`       | all         | Uses custom BatchJobs configuration script files, e.g. `.BatchJobs.R`            | N/A
| _predefined:_          |             |                                                                           |
| `batchjobs_torque`     | all         | Futures are evaluated via a [TORQUE] / PBS job scheduler                      | N/A
| `batchjobs_slurm`      | all         | Futures are evaluated via a [Slurm] job scheduler                           | N/A
| `batchjobs_sge`        | all         | Futures are evaluated via a [Sun/Oracle Grid Engine (SGE)] job scheduler    | N/A
| `batchjobs_lsf`        | all         | Futures are evaluated via a [Load Sharing Facility (LSF)] job scheduler     | N/A
| `batchjobs_openlava`   | all         | Futures are evaluated via an [OpenLava] job scheduler                       | N/A
| `batchjobs_interactive`| all         | synchronous evaluation in the calling R environment                       | `plan(transparent)`
| `batchjobs_local`      | all         | synchronous evaluation in a separate R process (on current machine)       | `plan(cluster, workers="localhost")`
| `batchjobs_multicore`  | not Windows | asynchronous background R sessions (on current machine)                   | `plan(multisession)` (sic!)


### Examples

Below are two examples illustrating how to use `batchjobs_custom` and `batchjobs_torque` to configure the BatchJobs backend.  For further details and examples on how to configure BatchJobs, see the [BatchJobs configuration] wiki page.

### Example: A .BatchJobs.R file using local BatchJobs
The most general way of configuring BatchJobs via a `.BatchJobs.R` file.
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

This follow the naming convention set up by the BatchJobs package.



### Example: A .BatchJobs.*.brew template file for TORQUE / PBS
To configure BatchJobs for job schedulers we need to setup a template
file that is used to generate the script used by the scheduler.
This is what a template file for TORQUE / PBS may look like:
```sh
#PBS -N <%= job.name %>

## merge standard error and output
#PBS -j oe

## Run R:
R CMD BATCH --no-save --no-restore "<%= rscript %>" /dev/stdout
```
If this template is saved to file `.BatchJobs.torque.brew` in the
working directory or the user's home directory, then it will be
automatically located and loaded when doing:
```r
> plan(batchjobs_torque)
```
To specify this template file explicitly, one can use
```r
> plan(batchjobs_torque, pathname="./.BatchJobs.torque.brew")
```

A similar filename format is used for the other types of job schedulers supported.  For instance, for Slurm the template file should be named `.BatchJobs.slurm.brew` in order for
```r
> plan(batchjobs_slurm)
```
to locate the file automatically.


Note that it is still possible to use a `.BatchJobs.R` and load the template file using a standard BatchJobs approach for maximum control.  For further details and examples on how to configure BatchJobs per se, see the [BatchJobs configuration] wiki page.



## Demos
The [future] package provides a demo using futures for calculating a
set of Mandelbrot planes.  The demo does not assume anything about
what type of futures are used.
_The user has full control of how futures are evaluated_.
For instance, to use `local` BatchJobs futures, run the demo as:
```r
library("future.BatchJobs")
plan(batchjobs_local)
demo("mandelbrot", package="future", ask=FALSE)
```


[BatchJobs]: http://cran.r-project.org/package=BatchJobs
[brew]: http://cran.r-project.org/package=brew
[future]: http://cran.r-project.org/package=future
[future.BatchJobs]: http://cran.r-project.org/package=future.BatchJobs
[BatchJobs configuration]: https://github.com/tudo-r/BatchJobs/wiki/Configuration
[TORQUE]: https://en.wikipedia.org/wiki/TORQUE
[Slurm]: https://en.wikipedia.org/wiki/Slurm_Workload_Manager
[Sun/Oracle Grid Engine (SGE)]: https://en.wikipedia.org/wiki/Oracle_Grid_Engine
[Load Sharing Facility (LSF)]: https://en.wikipedia.org/wiki/Platform_LSF
[OpenLava]: https://en.wikipedia.org/wiki/OpenLava


## Installation
R package future.BatchJobs is only available via [GitHub](https://github.com/HenrikBengtsson/future.BatchJobs) and can be installed in R as:
```r
source('http://callr.org/install#HenrikBengtsson/future.BatchJobs')
```




## Software status

| Resource:     | GitHub        | Travis CI      | Appveyor         |
| ------------- | ------------------- | -------------- | ---------------- |
| _Platforms:_  | _Multiple_          | _Linux & OS X_ | _Windows_        |
| R CMD check   |  | <a href="https://travis-ci.org/HenrikBengtsson/future.BatchJobs"><img src="https://travis-ci.org/HenrikBengtsson/future.BatchJobs.svg" alt="Build status"></a>  | <a href="https://ci.appveyor.com/project/HenrikBengtsson/future-batchjobs"><img src="https://ci.appveyor.com/api/projects/status/github/HenrikBengtsson/future.BatchJobs?svg=true" alt="Build status"></a> |
| Test coverage |                     | <a href="https://codecov.io/gh/HenrikBengtsson/future.BatchJobs"><img src="https://codecov.io/gh/HenrikBengtsson/future.BatchJobs/branch/develop/graph/badge.svg" alt="Coverage Status"/></a>    |                  |
