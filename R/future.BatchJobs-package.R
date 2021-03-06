#' future.BatchJobs: A Future for BatchJobs
#'
#' The \pkg{future.BatchJobs} package implements the Future API
#' on top of \pkg{BatchJobs} such that futures can be resolved
#' on for instance high-performance compute (HPC) clusters via
#' job schedulers.
#' The Future API is defined by the \pkg{future} package.
#'
#' To use BatchJobs futures, load \pkg{future.BatchJobs}, and
#' select the type of future you wish to use via
#' \code{\link[future:plan]{plan}()}.
#'
#' @example incl/future.BatchJobs.R
#'
#' @examples
#' \donttest{
#' plan(batchjobs_local)
#' demo("mandelbrot", package="future", ask=FALSE)
#' }
#'
#' @docType package
#' @name future.BatchJobs
#' @aliases future.BatchJobs-package
NULL
