source("incl/start.R")

message("*** batchjobs() & backend() are defunct ...")

res <- tryCatch(backend(), error = identity)
stopifnot(inherits(res, "error"))

res <- tryCatch(f <- batchjobs({ 42 }), error = identity)
stopifnot(inherits(res, "error"))

res <- tryCatch(f <- batchjobs({ 42 }, backend="interactive"), error = identity)
stopifnot(inherits(res, "error"))

res <- tryCatch(x %<-% { 42 } %plan% batchjobs(backend="interactive"), error = identity)

message("*** batchjobs() & backend() are defunct ... DONE")

source("incl/end.R")
