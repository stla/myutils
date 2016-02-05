setwd("~/Github/myutils/inst/bratteli")

library(myutils)

Pascal <- function(n){
  M <- matrix(0, nrow=n+1, ncol=n+2)
  for(i in 1:(n+1)){
    M[i, ][c(i, i+1)] <- 1
  }
  return(M)
}

N <- 3
Mn <- MMn <- Bwalk(Pascal, N, 3)

fedgelabels <- Vectorize(function(n, from, to) as.character(gmp::as.bigq(nchar(colnames(MMn[[n+1]])[to]), nchar(rownames(MMn[[n+1]])[from]))))

BgraphTikZ("tikz_Bwalk2.tex", function(n) Mn[[n+1]], 3, 
           fedgelabels = fedgelabels, 
           ROOTLABEL=rownames(Mn[[1]]),
           mirror=TRUE, hor=TRUE)

