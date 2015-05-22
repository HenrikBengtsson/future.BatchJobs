#' Inspect an asynchroneous variable
#'
#' @param var the variable.
#' @param envir the environment where to search from.
#' @param inherits Search parent frames or not.
#'
#' @return If exists, an AsyncTask object, otherwise NA.
#'
#' @export
#' @importFrom listenv get_variable
inspect <- function(var=NULL, envir=parent.frame(), inherits=TRUE) {
  expr <- substitute(var)

  ## Inspect all elements in environment?
  if (is.null(expr)) {
    res <- lapply(seq_along(envir), FUN=function(idx) {
      expr <- substitute(inspect(idx, envir=envir, inherits=inherits),
              list(idx=idx, inherits=inherits))
      eval(expr)
    })
    names(res) <- names(envir)
    return(res)
  }

  if (is.character(expr) || is.numeric(expr)) {
    name <- expr
    if (length(name) > 1L) {
      stop(sprintf("Inspection can only be done on a single element at the time, not %d: %s", length(name), hpaste(name, collapse=", ")), call.=FALSE)
    }

    ## Special: listenv:s
    if (inherits(envir, "listenv")) {
      name <- get_variable(envir, name, mustExist=TRUE)
    }
  } else if (is.language(expr)) {
    n <- length(expr)
    name <- paste(deparse(expr), collapse="")
    if (n != 1L && n != 3L) {
      stop("Not a valid variable format: ", name, call.=FALSE)
    }
    if (n == 1L) {
      ## Assignment to a variable name
      if (!grepl("^[.a-zA-Z]", name)) {
        stop("Not a valid variable name: ", name, call.=FALSE)
      }

      if (!exists(name, envir=envir, inherits=TRUE)) {
        stop(sprintf("Object %s not found: %s", sQuote(name), name), call.=FALSE)
      }
      obj <- get(name, envir=envir, inherits=TRUE)

      ## Special case inspect(env) => inspect(NULL, envir=env)
      if (inherits(obj, "environment")) {
        return(inspect(NULL, envir=obj, inherits=inherits))
      }
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
        }

        if (!is.numeric(idx) && !is.character(idx)) {
          stop(sprintf("Invalid subset argument of type %s: %s", sQuote(typeof(idx)), name), call.=FALSE)
        }

        ## Validate subetting, i.e. the 'idx'
        if (length(idx) > 1L) {
          stop(sprintf("Inspection can only be done on a single element at the time, not %d: %s", length(idx), name), call.=FALSE)
        }

        ## Special: listenv:s
        if (inherits(obj, "listenv")) {
          ## Get variable name to use
          idx <- get_variable(obj, idx, mustExist=TRUE)
        }

        if (is.character(idx)) {
        } else if (is.numeric(idx)) {
          stop(sprintf("Inspection with indexed subsetting can not be done on a %s: %s", sQuote(mode(obj)), name), call.=FALSE)
        } else {
          stop(sprintf("Invalid subset %s: %s", sQuote(deparse(idx)), name), call.=FALSE)
        }

        if (is.environment(obj)) {
          name <- idx
          envir <- obj
        } else {
          stop(sprintf("Inspection can not be done to a %s; only to a variable or an environment: %s", sQuote(mode(obj)), name), call.=FALSE)
        }
      }
    } # if (n == 3)
  }

  taskname <- sprintf(".task_%s", name)
  if (exists(taskname, mode="list", envir=envir, inherits=inherits)) {
    return(get(taskname, mode="list", envir=envir, inherits=inherits))
  }

  NA
} # inspect()

