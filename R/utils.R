isNA <- function(x) {
  if (length(x) != 1L) return(FALSE)
  is.na(x)
}
