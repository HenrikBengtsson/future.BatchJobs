source("incl/start.R")

message("*** Random Number Generation (RNG) ...")

## See Section 6 on 'Random-number generation' in
## vignette("parallel", package = "parallel")
fsample <- function(x, size = 4L, seed = NULL, what = c("future", "%<-%"), lazy = FALSE) {
  what <- match.arg(what)
  
  ## Must use session-specific '.GlobalEnv' here
  .GlobalEnv <- globalenv()
  
  oseed <- .GlobalEnv$.Random.seed
  orng <- RNGkind("L'Ecuyer-CMRG")[1L]
  on.exit(RNGkind(orng))

  if (!is.null(seed)) {
    ## Reset state of random seed afterwards?
    on.exit({
      if (is.null(oseed)) {
        rm(list = ".Random.seed", envir = .GlobalEnv, inherits = FALSE)
      } else {
        .GlobalEnv$.Random.seed <- oseed
      }
    }, add = TRUE)

    set.seed(seed)
  }

  .seed <- .Random.seed

  if (what == "future") {
    fs <- list()
    for (ii in seq_len(size)) {
      .seed <- parallel::nextRNGStream(.seed)
      fs[[ii]] <- future({ sample(x, size = 1L) }, lazy = lazy, seed = .seed)
    }
    res <- values(fs)
  } else {
    res <- listenv::listenv()
    for (ii in seq_len(size)) {
      .seed <- parallel::nextRNGStream(.seed)
      res[[ii]] %<-% { sample(x, size = 1L) } %lazy% lazy %seed% .seed
    }
    res <- as.list(res)
  }
  
  res
} # fsample()

 
dummy <- sample(0:2, size = 1L)
seed0 <- .Random.seed

## Reference sample with fixed random seed
y0 <- local({
  print(unclass(plan))
  utils::str(plan)
  old_plan <- plan()
  plan("sequential")
  on.exit(plan(old_plan))
  fsample(0:2, seed = 42L)
})

## Assert that random seed is reset
stopifnot(identical(.GlobalEnv$.Random.seed, seed0))

for (lazy in c(FALSE, TRUE)) {
  for (what in c("future", "%<-%")) {
    message(sprintf("- lazy = %s, what = %s", lazy, sQuote(what)))
    
    .GlobalEnv$.Random.seed <- seed0
  
    ## Fixed random seed
    y1 <- fsample(0:2, seed = 42L, what = what, lazy = lazy)
    print(y1)
    stopifnot(identical(y1, y0))
  
    ## Assert that random seed is reset
    stopifnot(identical(.GlobalEnv$.Random.seed, seed0))
  
    ## Fixed random seed
    y2 <- fsample(0:2, seed = 42L, what = what, lazy = lazy)
    print(y2)
    stopifnot(identical(y2, y1))
    stopifnot(identical(y2, y0))
  
    ## Assert that random seed is reset
    stopifnot(identical(.GlobalEnv$.Random.seed, seed0))
  
    ## No seed
    y3 <- fsample(0:2, what = what, lazy = lazy)
    print(y3)
  
    ## No seed
    y4 <- fsample(0:2, what = what, lazy = lazy)
    print(y4)
  } ## for (what ...)
} ## for (lazy ...)

message("*** Random Number Generation (RNG) ... DONE")

source("incl/end.R")
