source("incl/start.R")

message("*** BatchJobsFuture() ...")

task <- BatchJobsFuture({ x <- 1 })
print(task)

try(print(delete(task)))
try(print(delete(task)))

message("*** BatchJobsFuture() ... DONE")

source("incl/end.R")
