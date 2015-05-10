#' Evaluate multiple R expressions asynchronously
#'
#' @param exprs A \link[base]{list} of R \link[base]{expression}s.
#' @param ... Additional arguments passed to \code{asyncBatchEvalQ)}.
#' @param envir The \link[base]{environment} from where to search
#' for global variables.
#'
#' @return A \code{\link{listenv}} of length \code{length(exprs)}.
#'
#' @section Best practice for using substitute():
#' If using \code{substitute()} to create expressions, as in
#' the example below, it is recommended to \emph{always} specify
#' argument \code{env} even if no substitutions are indendent, i.e.
#' \code{expr <- substitute(..., env=list())}.
#' This is because the default value of \code{env} differs when
#' called from the \emph{global environment} and other environments.
#' For details on this unusual behavior, see the help on
#' \code{\link{substitute}()}.
#'
#' @example inst/asyncEvalQ.R
#'
#' @seealso \code{\link{delayedAsyncAssign}()} and
#' its corresponding operator \code{\link{\%<=\%}}.
#'
#' @export
#' @importFrom R.utils mcat mprint mprintf mstr
#' @keywords internal
asyncEvalQ <- function(exprs, ..., envir=parent.frame()) {
  nexprs <- length(exprs)
  env <- listenv(length=nexprs)
  names(env) <- names(exprs)

  for (ii in seq_len(nexprs)) {
    var <- get_variable(env, ii)
    expr <- exprs[[ii]]
    delayedAsyncAssign(var, expr, envir=envir, assign.env=env)
  }

  env
} # asyncEvalQ()
