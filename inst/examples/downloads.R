R.utils::use()
use("async")

url <- "http://www.r-project.org"

## Download front page
front %<=% downloadFile(url)

## Download subpages
pages %<=% {
  mprint(url)
  pattern <- '.*"(.*[.]html)".*'
  files <- gsub(pattern, "\\1", grep(pattern, readLines(front), value=TRUE))
  print(files)

  pages <- new.env()
  for (file in files) {
    print(url)
    pages[[file]] %<=% downloadFile(file.path(url, file))
  }
  ls(envir=pages)
}

