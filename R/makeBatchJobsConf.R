makeBatchJobsConf <- function(cluster.functions, ...) {
  getBatchJobsConf <- importBatchJobs("getBatchJobsConf")

  conf <- getBatchJobsConf()

  conf$cluster.functions <- cluster.functions
  conf$mail.start <- "none"
  conf$mail.done <- "none"
  conf$mail.error <- "none"
  conf$db.driver <- "SQLite"
  conf$db.options <- list()
  conf$default.resources <- list()
  conf$debug <- FALSE
  conf$raise.warnings <- FALSE
  conf$staged.queries <- TRUE
  conf$max.concurrent.jobs <- Inf
  conf$fs.timeout <- NA_real_

  ## Sanity check
  stopifnot(is.environment(conf))

  conf
} ## makeBatchJobsConf()
