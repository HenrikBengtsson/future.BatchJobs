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
#' @importFrom listenv map parse_env_subset
inspect <- function(var=NULL, envir=parent.frame(), mustExist=FALSE) {
  res <- NA_character_

  get_future <- function(target) {
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

    future_name <- sprintf(".future_%s", name)
    if (!exists(future_name, mode="list", envir=envir, inherits=FALSE)) {
      msg <- sprintf("Future (%s) not found in %s: %s", sQuote(future_name), sQuote(class(envir)[1]), sQuote(target$code))
      if (mustExist) stop(msg, call.=FALSE)
      attr(res, "reason") <- msg
      return(res)
    }

    get(future_name, mode="list", envir=envir, inherits=FALSE)
  } # get_future()

  expr <- substitute(var)

  ## Inspect all elements in environment?
  if (is.null(expr)) {
    res <- lapply(seq_along(envir), FUN=function(idx) {
      target <- parse_env_subset(idx, envir=envir, substitute=FALSE)
      get_future(target)
    })
    names(res) <- names(envir)
    return(res)
  }

  target <- parse_env_subset(expr, envir=envir, substitute=FALSE)
  get_future(target)
}
