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
inspect <- function(var, envir=parent.frame(), inherits=TRUE) {
  expr <- substitute(var)
  if (is.language(expr)) {
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
          stop(sprintf("Inspection can only be done on a single element at the time, not %d: %s", length(idx), name), call.=FALSE)
        }

        ## Special: listenv:s
        if (inherits(obj, "listenv")) {
          ## Get variable name to use
          idx <- get_variable(obj, idx, create=FALSE)
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

  ## All elements of an listenv is inside the object
#  if (inherits(envir, "listenv")) inherits <- FALSE

  taskname <- sprintf(".task_%s", name)
  if (exists(taskname, mode="list", envir=envir, inherits=inherits)) {
    return(get(taskname, mode="list", envir=envir, inherits=inherits))
  }

  NA
}
