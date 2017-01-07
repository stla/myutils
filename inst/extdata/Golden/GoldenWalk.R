setwd("~/Github/myutils/inst/extdata/Golden")


Golden_Mn <- function(n){
  if(n==0) return(t(c(1,1)))
  M <- matrix(1, nrow=2, ncol=2)
  if(n%%2==0) M[2,1] <- 0 else M[1,2] <- 0
  if(n%%2==1) colnames(M) <- c(0,1) else colnames(M) <- c(1,0) 
  return(unname(M))
}

N <- 4
Mn <- Bwalk_powers(Golden_Mn, N, v=2, labels="words")

BgraphTikZ("GoldenWalk_R.tex", function(n) Mn$Mn[[n+1]], N,
           mirror=TRUE, ROOTLABEL=rownames(Mn$Mn[[1]])) 
