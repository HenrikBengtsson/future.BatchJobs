<%
## Reuse the future vignette
md <- R.rsp::rstring(file="vignettes/future.BatchJobs.md.rsp", postprocess=FALSE)

## Drop the header
md <- unlist(strsplit(md, split="\n", fixed=TRUE))
md <- md[-seq_len(grep("^## ", md)[1])]

## Drop the footer
md <- md[seq_len(grep("^---", md)[1]-1)]

md <- c("![Life cycle: superseded](vignettes/imgs/lifecycle-superseded-blue.svg)", md)

## Output
cat(md, sep="\n")
%>
