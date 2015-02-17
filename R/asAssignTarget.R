## Helper function for %<-%, %<=%, ...
.asAssignTarget <- function(expr, envir=parent.frame()) {
  res <- list(envir=envir, name=NULL)

  if (is.symbol(expr)) {
    ## Assignment to variable specified as a symbol
    name <- deparse(expr)
    res$name <- name
  } else {
    n <- length(expr)
    name <- paste(deparse(expr), collapse="")
    if (n != 1L && n != 3L) {
      stop("Not a valid variable name for delayed assignments: ", name, call.=FALSE)
    }

    if (n == 1L) {
      ## Assignment to a variable name
      if (!grepl("^[.a-zA-Z]", name)) {
        stop("Not a valid variable name: ", name, call.=FALSE)
      }
      res$name <- name
    } else if (n == 3L) {
      ## Assignment to enviroment via $ and [[
      op <- expr[[1]]
      if (op == "$" || op == "[[") {
        ## Target
        objname <- deparse(expr[[2]])
        if (!exists(objname, envir=envir, inherits=TRUE)) {
          stop(sprintf("Object %s not found: %s", sQuote(objname), name), call.=FALSE)
        }
        obj <- get(objname, envir=envir, inherits=TRUE)

        ## Subset
        idx <- expr[[3]]
        if (is.symbol(idx)) {
          idx <- deparse(idx)
          if (op == "[[") {
            if (!exists(idx, envir=envir, inherits=TRUE)) {
              stop(sprintf("Object %s not found: %s", sQuote(idx), name), call.=FALSE)
            }
            idx <- get(idx, envir=envir, inherits=TRUE)
          }
        } else if (is.language(idx)) {
          idx <- eval(idx, envir=envir)
        }

        ## Validate subetting, i.e. the 'idx'
        if (length(idx) > 1L) {
          stop(sprintf("Delayed assignments with subsetting can only be done on a single element at the time, not %d: %s", length(idx), name), call.=FALSE)
        }

        if (is.character(idx)) {
        } else if (is.numeric(idx)) {
          if (inherits(obj, "listenv")) {
            ## Get variable name to use
            idx <- get_variable(obj, idx)
          } else {
            stop(sprintf("Delayed assignments with indexed subsetting can not be done on a %s; only for listenv: %s", sQuote(mode(obj)), name), call.=FALSE)
          }
        } else {
          stop(sprintf("Invalid subset %s: %s", sQuote(deparse(idx)), name), call.=FALSE)
        }

        if (is.environment(obj)) {
          res$name <- idx
          res$envir <- obj
        } else {
          stop(sprintf("Delayed assignments can not be done to a %s; only to a variable or an environment: %s", sQuote(mode(obj)), name), call.=FALSE)
        }
      } else {
        stop("Not a valid target for a delayed assignment: ", name, call.=FALSE)
      }
    }
  }

  ## Sanity check
  stopifnot(is.environment(res$envir))
  stopifnot(is.character(res$name))

  res
}
