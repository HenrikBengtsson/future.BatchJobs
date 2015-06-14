source("incl/start.R")

message("*** availableCores() ...")

print(availableCores())
print(availableCores("PBS"))
print(availableCores("system"))

message("*** availableCores() ... DONE")

source("incl/end.R")
