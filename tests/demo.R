library("async")

ovars <- ls()
oopts <- options(warn=1L)

options("R_FUTURE_DEMO_MANDELBROT_PLANES"=4L)

message("*** Demos ...")

message("*** Mandelbrot demo of the 'future' package ...")

plan(batchjobs, batchjobs="local")
demo("mandelbrot", package="future", ask=FALSE)

message("*** Demos ... DONE")

plan(eager)
options(oopts)
rm(list=setdiff(ls(), ovars))
