## Helper function for %<-%, %<=%, ...
#' @importFrom listenv listenv get_variable
asAssignTarget <- function(expr, envir=parent.frame(), substitute=FALSE) {
  if (substitute) expr <- substitute(expr)

  res <- list(envir=envir, name="", idx=NA_integer_, exists=NA)

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
        if (length(idx) != 1L) {
          stop(sprintf("Delayed assignments with subsetting can only be done on a single element at the time, not %d: %s", length(idx), name), call.=FALSE)
        }

        ## Special: listenv:s
        if (inherits(obj, "listenv")) {
          names <- names(obj)
          if (is.numeric(idx)) {
            res$idx <- idx
            res$exists <- (idx >= 1 && idx <= length(obj))
            idx <- names[idx]
            if (length(idx) == 0L) idx <- ""
          } else if (is.character(idx)) {
            if (!nzchar(idx)) {
              stop("Invalid indexing. Index must not be an empty name.")
            }
            res$idx <- match(idx, names)
            res$exists <- !is.na(res$idx)
          }
        }

        if (is.character(idx)) {
          res$name <- idx
        } else if (is.numeric(idx)) {
          stop(sprintf("Delayed assignments with indexed subsetting can not be done on a %s: %s", sQuote(mode(obj)), name), call.=FALSE)
        } else {
          stop(sprintf("Invalid subset %s: %s", sQuote(deparse(idx)), name), call.=FALSE)
        }

        if (is.environment(obj)) {
          res$envir <- obj
        } else {
          stop(sprintf("Delayed assignments can not be done to a %s; only to a variable or an environment: %s", sQuote(mode(obj)), name), call.=FALSE)
        }
      } else {
        stop("Not a valid target for a delayed assignment: ", name, call.=FALSE)
      }
    }
  }

  if (is.na(res$exists)) {
    res$exists <- exists(res$name, envir=res$envir, inherits=TRUE)
  }

  ## Sanity check
  stopifnot(is.environment(res$envir))
  stopifnot(is.character(res$name))
  stopifnot(is.null(res$idx) || is.numeric(res$idx))
  stopifnot(is.logical(res$exists), !is.na(res$exists))

  res
}
