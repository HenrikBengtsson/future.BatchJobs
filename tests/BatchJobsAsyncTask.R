source("incl/start.R")

message("*** BatchJobsAsyncTask() ...")

task <- BatchJobsAsyncTask({ x <- 1 })
print(task)

try(print(delete(task)))
try(print(delete(task)))

message("*** BatchJobsAsyncTask() ... DONE")

source("incl/end.R")
