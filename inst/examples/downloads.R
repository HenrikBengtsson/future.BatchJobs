R.utils::use()
use("async")

url <- "http://www.r-project.org"

## Download front page
front %<=% {
  cat("URL:\n")
  print(url)
  urlT <- file.path(url, "index.html")
  downloadFile(urlT)
}
print(front)

## Download subpages
pages %<=% {
  url2 <- "http://www.r-project.org"

  pattern <- '.*"(.*[.]html)".*'
  files <- gsub(pattern, "\\1", grep(pattern, readLines(front), value=TRUE))
  print(files)

  pages <- new.env()
  for (file in files) {
    print(url)
    urlT <- file.path(url2, file)
    print(urlT)
    pages[[file]] %<-% downloadFile(urlT)
  }
  ls(envir=pages)
}
print(pages)
