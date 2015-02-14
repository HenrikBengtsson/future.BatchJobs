R.utils::use()
use("async")
backend("local")

tmpdir <- "downloads"
mkdirs(tmpdir)
opwd <- setwd(tmpdir)


url <- "http://www.r-project.org"

## Download front page
front %<=% {
  cat("URL:\n")
  print(url)
  urlT <- file.path(url, "index.html")
  downloadFile(urlT)
}


## Download subpages
pages %<=% {
  url2 <- "http://www.r-project.org"

  pattern <- '.*"(.*[.]html)".*'
  files <- gsub(pattern, "\\1", grep(pattern, readLines(front), value=TRUE))
  files <- unique(files)
  print(files)

  pages <- new.env()
  for (file in files) {
    urlT <- file.path(url2, file)
    pages[[file]] %<=% downloadFile(urlT)
  }
  print(ls(envir=pages))
  mget(files, envir=pages)
}
print(pages)

setwd(opwd)

