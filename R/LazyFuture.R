#' A lazy future represents a \link{Future} whose value will be resolved at the time when it is requested
#'
#' @param object An R \link[base]{environment}.
#' @param ... Not used.
#'
#' @return An object of class \code{LazyFuture}.
#'
#' @seealso
#' To evaluate an expression using "lazy future", see function
#' \code{\link{lazyfuture}()}, or alternatively, use the
#' infix assignment operator \code{\link{\%<-\%}}.
#'
#' @export
#' @name LazyFuture-class
LazyFuture <- function(object=new.env(parent=emptyenv()), ...) {
  if (!is.environment(object)) {
    stop(sprintf("Argument 'object' is not an environment: ", class(object)))
  }
  Future(structure(object, class=c("LazyFuture", class(object))))
}


#' @export
value.LazyFuture <- function(future, onCondition=c("signal", "return"), ...) {
  get("value", envir=future, inherits=FALSE)
}

#' @export
isResolved.LazyFuture <- function(future, ...) {
  value(future, onCondition="return")
  TRUE
}

#' A lazy future represents a future whose value will be resolved at the time when it is requested
#'
#' A lazy future is a future that uses lazy evaluation, which means
#' that its value is only computed and resolved at the time when the
#' value is requested.  This means that the future will not be resolved
#' if the value is never requested.
#'
#' @param expr An R \link[base]{expression}.
#' @param envir The \link{environment} in which the evaluation
#' is done (or inherits from if \code{local} is TRUE).
#' @param substitute If TRUE, argument \code{expr} is
#' \code{\link[base]{substitute}()}:ed, otherwise not.
#' @param local If TRUE, the expression is evaluated such that
#' all assignments are done to local temporary environment, otherwise
#' the assignments are done in the calling environment.
#' @param ... Not used.
#'
#' @return A \link{LazyFuture}.
#'
#' @example incl/future.R
#'
#' @details
#' This function can be registered as the default \link{future} evaluator,
#' i.e. \code{options(future=lazyfuture)}.
#'
#' @seealso Internally, \code{\link[base]{delayedAssign}()} is utilized to
#' create a "\emph{\link[base]{promise}}", which hold the future's value.
#'
#' @export
#' @name lazyfuture
lazyfuture <- function(expr, envir=parent.frame(), substitute=TRUE, local=TRUE, ...) {
  if (substitute) expr <- substitute(expr)

  ## Evaluate in local() environment?
  if (local) {
    a <- NULL; rm(list="a"); # To please R CMD check
    expr <- substitute(local(a), list(a=expr))
  }

  future <- LazyFuture()
  delayedAssign("value", eval(expr, envir=envir), assign.env=future)

  future
}
