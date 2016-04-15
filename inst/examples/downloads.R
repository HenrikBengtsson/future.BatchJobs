library("async")

tmpdir <- "downloads"
mkdirs(tmpdir)
opwd <- setwd(tmpdir)


url <- "http://www.r-project.org"

## Download front page
front %<-% downloadFile(file.path(url, "index.html"))

## Download subpages
pages %<-% {
  pattern <- '.*"(.*[.]html)".*'
  files <- gsub(pattern, "\\1", grep(pattern, readLines(front), value=TRUE))
  files <- unique(files)
  print(files)

  pages <- new.env()
  for (file in files) {
    pages[[file]] %<-% downloadFile(file.path(url, file))
  }

  # Don't forget to retrieve the "files"!
  mget(files, envir=pages)
}
print(pages)

setwd(opwd)

