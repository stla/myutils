setwd("~/Github/myutils/inst/extdata/templates")

tt <- paste(readLines("template_BratteliTikZ3.tex"), collapse="\n")
saveRDS(tt, file="../../templates/template_BratteliTikZ3.RDS")

