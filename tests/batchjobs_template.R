source("incl/start.R")

message("*** batchjobs_template() ...")

## NOTE: Here we use invalid 'workers=FALSE' in order to
## prevent the BatchJobs future from actually starting,
## because we cannot assume that system has these schedulers.
## NOTE: Some of them will give an earlier error because
## no default template file was found.
res <- try(batchjobs_lsf({ 42L }, workers=FALSE))
stopifnot(inherits(res, "try-error"))

res <- try(batchjobs_openlava({ 42L }, workers=FALSE))
stopifnot(inherits(res, "try-error"))

res <- try(batchjobs_sge({ 42L }, workers=FALSE))
stopifnot(inherits(res, "try-error"))

res <- try(batchjobs_slurm({ 42L }, workers=FALSE))
stopifnot(inherits(res, "try-error"))

res <- try(batchjobs_torque({ 42L }, workers=FALSE))
stopifnot(inherits(res, "try-error"))



## NOTE: Here we use invalid 'args' in order to
## prevent the BatchJobs future from actually starting,
## because we cannot assume that system has these schedulers.
## These tests goes a little bit beyond the above ones
## and actually creates BatchJobs registries.
## NOTE: Some of them will give an earlier error because
## no default template file was found.

resources <- list(walltime=3600)
args <- list(
  non_supported="Gives error because BatchJobs only supports 'resources'"
)
  
res <- try(batchjobs_lsf({ 42L }, resources=resources, args=args))
stopifnot(inherits(res, "try-error"))

res <- try(batchjobs_openlava({ 42L }, resources=resources, args=args))
stopifnot(inherits(res, "try-error"))

res <- try(batchjobs_sge({ 42L }, resources=resources, args=args))
stopifnot(inherits(res, "try-error"))

res <- try(batchjobs_slurm({ 42L }, resources=resources, args=args))
stopifnot(inherits(res, "try-error"))

res <- try(batchjobs_torque({ 42L }, resources=resources, args=args))
stopifnot(inherits(res, "try-error"))

message("*** batchjobs_template() ... DONE")

source("incl/end.R")
