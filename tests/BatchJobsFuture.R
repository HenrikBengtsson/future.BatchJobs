source("incl/start.R")

message("*** BatchJobsFuture() ...")

f <- BatchJobsFuture({ x <- 1 })
print(f)

try(print(delete(f)))
try(print(delete(f)))

message("*** BatchJobsFuture() ... DONE")

source("incl/end.R")
