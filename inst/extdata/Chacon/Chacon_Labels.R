setwd("/home/stla/Github/myutils/inst/extdata/Chacon")

Chacon <- function(n){
  if(n==0) return(t(c(1L,1L)))
  return(t(matrix(c(3L, 1L, 0L, 1L), byrow=TRUE, nrow=2)))
}

N <- 4

#### Graphe avec les labels ####

fedgelabels <- Vectorize(function(n, from, to, mindex){
  if(n==0) return(".")
  if(from==2) return(ifelse(to==1, "2", "0")) 
  if(mindex %in% c(1,2)) return(mindex-1) else return(3)
})

myutils::BgraphTikZ("Chacon_Labels_R.tex", Chacon, N=N, 
                    bending=2,
                    edgelabels=fedgelabels)

myutils::BgraphTikZ("Chacon_Labels_colorpath_R.tex", Chacon, N=N, 
                    bending=2,
                    edgelabels=fedgelabels, 
                    colorpath=1)


