## Restore original state
options(oopts)
backend(obackend)
plan(oplan)
rm(list=c(setdiff(ls(), ovars)))

