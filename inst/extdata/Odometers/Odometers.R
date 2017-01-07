setwd("~/Github/myutils/inst/extdata/Odometers")
library(myutils)

# simple edged
Odometer_Mn <- function(sizes){
  sizes <- c(1,sizes)
  function(n){
    return(matrix(1, nrow=sizes[n+1], ncol=sizes[n+2]))
  }
}

# base plot
par(mar=c(.1,.1,.1,.1))
fun_Mn <- Odometer_Mn(c(3,4,5))
Bgraph(fun_Mn, N=3, labels_vertex=TRUE, path=c(2,1,2), labels_path=TRUE)


# multiple edges
MnFun <- function(n){
  switch(as.character(n), 
         "0"=matrix(3),
         "1"=matrix(4),
         "2"=matrix(7))
}

#
myutils::BgraphTikZ("OdometerGraphMultiplesEdges_R.tex", MnFun, N=3,
                    vertexlabels = "dims", hor=TRUE, bending = 1.2)

# color path
xx <- myutils::BgraphTikZ("OdometerGraphMultiplesEdges_colorpath_R.tex", MnFun, N=3,
                    vertexlabels = "dims", hor=TRUE, bending = 1.2,
                    colorpath=52)

tools::texi2pdf("OdometerGraphMultiplesEdges_colorpath_R.tex", clean=TRUE)
