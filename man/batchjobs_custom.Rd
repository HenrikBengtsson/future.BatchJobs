% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/batchjobs_custom.R
\name{batchjobs_custom}
\alias{batchjobs_custom}
\title{BatchJobs conf futures}
\usage{
batchjobs_custom(
  expr,
  envir = parent.frame(),
  substitute = TRUE,
  globals = TRUE,
  label = "BatchJobs",
  conf = NULL,
  pathname = NULL,
  workers = Inf,
  resources = list(),
  job.delay = FALSE,
  ...
)
}
\arguments{
\item{expr}{The R expression to be evaluated}

\item{envir}{The environment in which global environment
should be located.}

\item{substitute}{Controls whether \code{expr} should be
\code{substitute()}:d or not.}

\item{globals}{(optional) a logical, a character vector, a named list, or a \link[globals]{Globals} object.  If TRUE, globals are identified by code inspection based on \code{expr} and \code{tweak} searching from environment \code{envir}.  If FALSE, no globals are used.  If a character vector, then globals are identified by lookup based their names \code{globals} searching from environment \code{envir}.  If a named list or a Globals object, the globals are used as is.}

\item{label}{(optional) Label of the future (where applicable, becomes the job name for most job schedulers).}

\item{conf}{A BatchJobs configuration environment.}

\item{pathname}{(alternative) Pathname to one or more BatchJobs
configuration files to be loaded in order.  If NULL, then the
\pkg{BatchJobs} package will search for such configuration files.}

\item{workers}{(optional) Additional specification for the backend
workers.  If NULL, the default is used.}

\item{resources}{A named list passed to the BatchJobs template (available as variable \code{resources}).}

\item{job.delay}{(optional) Passed as is to \code{\link[BatchJobs]{submitJobs}()}.}

\item{\ldots}{Additional arguments passed to \code{\link{BatchJobsFuture}()}.}
}
\value{
An object of class \code{BatchJobsFuture}.
}
\description{
A conf BatchJobs future sources one or more BatchJobs configuration
files (R source scripts) to define the BatchJobs configuration
environment, e.g. \file{.BatchJobs.R}.
}
\details{
If \code{conf} is NULL (default), then the BatchJobs configuration will
be created from a set of BatchJobs configuration files (R script files)
as given by argument \code{pathname}.  If none are specified (default),
then \pkg{BatchJobs} is designed to use (in order) all of following
configuration files (if they exist):
\itemize{
 \item \code{system("etc", "BatchJobs_global_config.R", package="BatchJobs")}
 \item \code{~/.BatchJobs.R} (in user's home directory)
 \item \code{.BatchJobs.R} (in the current directory)
}
}
