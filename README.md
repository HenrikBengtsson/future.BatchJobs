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
> plan(batchjobs)
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
separate chromosomes.  Here is an outline of how this nested parallelism
could be implemented using futures.
```r
library("future")
library("listenv")
## The first level of futures should be submitted to the
## cluster using BatchJobs.  The second level of futures
## should be using multiprocessing, where the number of
## parallel processes is automatically decided based on
## what the cluster allots to each compute node.
plan(list(batchjobs, multiprocess))

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
The future.BatchJobs package implements a generic future wrapper for all
BatchJobs backends and by default it looks for a `.BatchJobs.R`
configuration file and if not found it falls back to multicore
processing if supported, otherwise single-core processing.
Below are the most common types of BatchJobs backends.


| Backend                | OSes        | Description                                                               | Alternative in future package
|:-----------------------|:------------|:--------------------------------------------------------------------------|:------------------------------------
| _generic:_             |             |                                                                           |
| `batchjobs_conf`       | all         | Uses BatchJobs configuration script files, e.g. `.BatchJobs.R`            | N/A
| _predefined:_          |             |                                                                           |
| `batchjobs_torque`     | all         | Futures are evaluated via a [TORQUE] / PBS job scheduler                      | N/A
| `batchjobs_slurm`      | all         | Futures are evaluated via a [Slurm] job scheduler                           | N/A
| `batchjobs_sge`        | all         | Futures are evaluated via a [Sun/Oracle Grid Engine (SGE)] job scheduler    | N/A
| `batchjobs_lsf`        | all         | Futures are evaluated via a [Load Sharing Facility (LSF)] job scheduler     | N/A
| `batchjobs_openlava`   | all         | Futures are evaluated via an [OpenLava] job scheduler                       | N/A
| `batchjobs_interactive`| all         | synchronous evaluation in the calling R environment                       | `plan(transparent)`
| `batchjobs_local`      | all         | synchronous evaluation in a separate R process (on current machine)       | `plan(cluster, workers="localhost")`
| `batchjobs_multicore`  | not Windows | asynchronous background R sessions (on current machine)                   | `plan(multisession)` (sic!)


### Example: A `.BatchJobs.R` file for TORQUE/PBS
The most powerful and most common usage of BatchJobs futures is via a
backend configured by a `.BatchJobs.R` file.  For example, to use
futures that are distributed on a compute cluster via a TORQUE/PBS job
scheduler, use:
```r
library("future.BatchJobs")
plan(batchjobs_conf)
```
and then use a `.BatchJobs.R` file (in the working directory or in
your home directory) with the following content:
```r
cluster.functions <- makeClusterFunctionsTorque(R.utils::tmpfile('
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
```
For further details and examples on how to configure BatchJobs, see
the [BatchJobs configuration] wiki page.



## Demos
The [future] package provides a demo using futures for calculating a
set of Mandelbrot planes.  Except from using futures, the demo does
not assume anything about what type of futures are used.
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
[future.BatchJobs]: https://github.com/HenrikBengtsson/future.BatchJobs/
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
