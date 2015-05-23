#' Inspect an asynchroneous variable
#'
#' @param var the variable.
#' @param envir the environment where to search from.
#' @param mustExist If TRUE and the variable does not exists, then
#' an informative error is thrown, otherwise NA is returned.
#'
#' @return An AsyncTask object (or NA).
#'
#' @export
#' @importFrom listenv map
inspect <- function(var=NULL, envir=parent.frame(), mustExist=FALSE) {
  res <- NA_character_

  get_task <- function(target) {
    if (!target$exists) {
      msg <- sprintf("Variable not found: %s", target$code)
      if (mustExist) stop(msg, call.=FALSE)
      attr(res, "reason") <- msg
      return(res)
    }

    envir <- target$envir
    if (inherits(envir, "listenv")) {
      name <- map(envir)[target$idx]
    } else {
      name <- target$name
    }

    taskname <- sprintf(".task_%s", name)
    if (!exists(taskname, mode="list", envir=envir, inherits=FALSE)) {
      msg <- sprintf("Task (%s) not found in %s: %s", sQuote(taskname), sQuote(class(envir)[1]), sQuote(target$code))
      if (mustExist) stop(msg, call.=FALSE)
      attr(res, "reason") <- msg
      return(res)
    }

    get(taskname, mode="list", envir=envir, inherits=FALSE)
  } # get_task()

  expr <- substitute(var)

  ## Inspect all elements in environment?
  if (is.null(expr)) {
    res <- lapply(seq_along(envir), FUN=function(idx) {
      target <- parseEnvSubset(idx, envir=envir, substitute=FALSE)
      get_task(target)
    })
    names(res) <- names(envir)
    return(res)
  }

  target <- parseEnvSubset(expr, envir=envir, substitute=FALSE)
  get_task(target)
}
