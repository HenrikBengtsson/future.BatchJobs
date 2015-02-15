#' @export
listenv <- function(length=0L) {
  .listenv.map <- character(0L)

  stopifnot(length >= 0L)
  .listenv.map <- rep(NA_character_, times=length)

  env <- new.env()
  class(env) <- c("listenv", class(env))
  env
}

#' @export
map <- function(...) UseMethod("map")

#' #' @export
map.listenv <- function(x) {
  get(".listenv.map", envir=x, inherits=TRUE)
}

#' @export
`map<-` <- function(...) UseMethod("map<-")

#' @export
`map<-.listenv` <- function(x, value) {
  stopifnot(is.character(value))
  assign(".listenv.map", value, envir=x, inherits=TRUE)
  invisible(x)
}

#' @export
vars <- function(...) UseMethod("vars")
vars.listenv <- function(x) {
  map(x)
}

#' @export
length.listenv <- function(x) {
  length(map(x))
}

#' @export
names.listenv <- function(x) {
  names(map(x))
}

#' @export
as.list.listenv <- function(x) {
  vars <- map(x)
  res <- vector("list", length=length(vars))
  names(res) <- names(x)
  ok <- !is.na(vars)
  res[ok] <- mget(vars[ok], envir=x, inherits=FALSE)
  res
}

#' @export
`names<-.listenv` <- function(x, value) {
  map <- map(x)
  if (length(value) != length(map)) {
    stop(sprintf("Number of names does not match the number of elments: %s != %s", length(value), length(map)))
  }
##  if (any(duplicated(value))) {
##    stop("Environments cannot have duplicate names on elements")
##  }
  names(map) <- value
  map(x) <- map
  invisible(x)
}

#' @export
`$.listenv` <- function(x, name) {
##  str(list(method="$<-", name=name))
  map <- map(x)
  var <- map[name]
  if (is.na(var)) return(NULL)

  get(var, envir=x, inherits=FALSE)
}


#' @export
`$<-.listenv` <- function(x, name, value) {
##  str(list(method="$<-", name=name, value=value))
  map <- map(x)

  ## Map to an existing or a new element?
  if (name %in% names(map)) {
    var <- map[name]

    ## A new variable?
    if (is.na(var)) {
      var <- name
      map[name] <- name
      map(x) <- map
    }
  } else {
    var <- name

    ## Append to map
    map <- c(map, var)
    names(map)[length(map)] <- var
    map(x) <- map
  }

  ## Assign value
  assign(var, value, envir=x, inherits=FALSE)

  invisible(x)
}


#' @export
`[[.listenv` <- function(x, i, ...) {
  map <- map(x)

##  str(list(method="[[", i=i))
  if (is.character(i)) {
    name <- i
    i <- match(name, table=names(map))
    if (is.na(i)) return(NULL)
  } else if (!is.numeric(i)) {
    return(NextMethod("[["))
  }

  if (length(i) != 1L) {
    stop("Subsetting of more than one element at the time is not allowed for listenv's: ", length(i))
  }

  n <- length(map)
  if (i < 1L || i > n) {
    stop(sprintf("Subscript out of bounds [%d,%d]: %d", min(0,n), n, i), call.=FALSE)
  }

  var <- map[i]

  ## Return default (NULL)?
  if (is.na(var) || !exists(var, envir=x, inherits=FALSE)) return(NULL)

  get(var, envir=x, inherits=FALSE)
}


#' @export
`[[<-.listenv` <- function(x, i, value) {
##  str(list(method="[[<-", i=i, value=value))
  if (length(i) != 1L) {
    stop("Subsetting of more than one element at the time is not allowed for listenv's: ", length(i))
  }

  if (is.character(i)) {
    x <- do.call(`$<-`, args=list(x, i, value), envir=parent.frame())
    return(invisible(x))
  } else if (is.symbol(i)) {
    name <- eval(i, envir=parent.frame())
    x <- do.call(`$<-`, args=list(x, name, value), envir=parent.frame())
    return(invisible(x))
  }

  if (!is.numeric(i)) {
    stop(sprintf("Subsetted [[<- assignment to listenv's is only supported for names and indices, not %s", mode(i)), ecall.=FALSE)
  }

  map <- map(x)
  n <- length(map)
  if (i < 1L) {
    stop(sprintf("Negative subscript out of bounds: %d", i), call.=FALSE)
  }

  ## Variable name
  var <- map[i]

  ## Non-existing variable?
  if (is.na(var)) {
    ## Expand map?
    if (i > n) {
      extra <- rep(NA_character_, times=i-n)
      map <- c(map, extra)
    }

    ## Create internal variable name
    var <- tempvar(value=value, envir=x, inherits=FALSE)
    map[i] <- var

    ## Update map
    map(x) <- map
  } else {
    assign(var, value, envir=x, inherits=FALSE)
  }

  invisible(x)
}


tempvar <- function(prefix="var", value, envir=parent.frame(), inherits=FALSE) {
  maxTries <- 1e+06
  maxInt <- .Machine$integer.max
  ii <- 0L
  while (ii < maxTries) {
    idx <- sample.int(maxInt, size=1L)
    name <- sprintf("%s%d", prefix, idx)
    if (!exists(name, envir=envir, inherits=inherits)) {
      if (!missing(value)) {
        assign(name, value, envir=envir, inherits=inherits)
      }
      return(name)
    }
    ii <- ii + 1L
  }
  stop(sprintf("Failed to generate a unique non-existing temporary variable with prefix '%s'", prefix), call.=FALES)
} # tempvar()
