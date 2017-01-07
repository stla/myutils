setwd("/home/stla/Github/myutils/inst/extdata/paths")

library(myutils)

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

xx <- myutils::BgraphTikZ("Chacon_Labels_R.tex", Chacon, N=N, 
                    bending=2,
                    edgelabels=fedgelabels)

dd <- as.data.frame(xx)[,1:8]

e0 <- which(dd$level==0) # edges level 0 to level 1
i <- e0[1]
j <- which(dd$node1 == dd$node2[i]) # edges connected to edge i
paths1 <- lapply(j, function(j) c(i,j))
k <- lapply(j, function(j) which(dd$node1 == dd$node2[j]))
paths2 <- lapply(seq_along(paths1), function(p) lapply(k[[p]], function(e) c(paths1[[p]], e)))
paths2 <- do.call(c, paths2)
l <- lapply(paths2, function(p) which(dd$node1 == dd$node2[tail(p,1)]))
paths3 <- lapply(seq_along(paths2), function(p) lapply(l[[p]], function(e) c(paths2[[p]], e)))
paths3 <- do.call(c, paths3)

# mieux 
i <- e0[1]
j <- which(dd$node1 == dd$node2[i]) # edges connected to edge i
paths1 <- lapply(j, function(j) c(i,j))
k <- lapply(paths1, function(p) which(dd$node1 == dd$node2[tail(p,1)]))
paths2 <- lapply(seq_along(paths1), function(p) lapply(k[[p]], function(e) c(paths1[[p]], e)))
paths2 <- do.call(c, paths2)
l <- lapply(paths2, function(p) which(dd$node1 == dd$node2[tail(p,1)]))
paths3 <- lapply(seq_along(paths2), function(p) lapply(l[[p]], function(e) c(paths2[[p]], e)))
paths3 <- do.call(c, paths3)

# boucle 
e0 <- which(dd$level==0)
f <- function(e){
  e1 <- which(dd$node1 == dd$node2[e]) # edges connected to edge i
  paths <- lapply(e1, function(j) c(e,j))
  if(N == 2){
    return(paths)
  }
  for(m in 2:(N-1)){
    e2 <- lapply(paths, function(p) which(dd$node1 == dd$node2[tail(p,1)]))
    paths <- do.call(c, lapply(seq_along(paths), function(p) lapply(e2[[p]], function(ee) c(paths[[p]], ee))))
  }
  return(paths)
}

do.call(c, lapply(e0, f))
