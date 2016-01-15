

# graphe de Bratteli ------------------------------------------------------
Mn_fun <- function(n){
  if(n==0) return(t(c(1,1)))
  M <- matrix(1, nrow=2, ncol=2)
  if(n%%2==0) M[2,1] <- 0 else M[1,2] <- 0
  return(M)
}
Bgraph(Mn_fun, N=5, points_vertex = TRUE, labels_vertex = FALSE) 

labs_edges = c(NA, NA, 1, 0, 0, 0, 0, 1, 1, 0, 0)
Bgraph(Mn_fun, N=5, points_vertex = TRUE, labels_vertex = FALSE, 
       labs_edges = labs_edges, 
       path=c(0,0,1,0,1)) 

# arbre des possibilités --------------------------------------------------
N <- 4L 
Mn <- vector(mode="list", length=N)
Mn[[1]] <- t(c(1,1))
Mn[[2]] <- myutils::blockdiag(Mn[[1]],as.matrix(1))
for(i in 3:N){
  # peut-être renverser des lignes - on verra
#   if(i%%2==1) Mn[[i]] <- myutils::blockdiag(Mn[[i-2]],Mn[[i-1]])
#   if(i%%2==0) Mn[[i]] <- myutils::blockdiag(Mn[[i-1]],Mn[[i-2]])
  Mn_m2 <- Mn[[i-2]]
  Mn_m1 <- Mn[[i-1]]
#   if(i%%2==1) Mn[[i]] <- myutils::blockdiag(Mn_m1[rev(seq_len(nrow(Mn_m1))),],Mn_m2)
#   if(i%%2==0) Mn[[i]] <- myutils::blockdiag(Mn[[i-1]],Mn[[i-2]])
  Mn[[i]] <- myutils::blockdiag(Mn[[i-1]],Mn[[i-2]])
}

Bgraph(function(n) Mn[[n+1]], N=3, labels_vertex = FALSE) # path=c(1,0,1), first_vertex=1, hor=FALSE)

colnames(Mn[[1]]) <- c("abcde", "fgh")
colnames(Mn[[2]]) <- c("abc", "de", "fgh")
colnames(Mn[[3]]) <- c("ab", "c", "de", "fg", "h")
colnames(Mn[[4]]) <- c("a", "b", "c", "d", "e", "f", "g", "h")
Bgraph(function(n) Mn[[n+1]], N=4, USE.COLNAMES = TRUE, label_root = "abcdefgh")

labs_edges <- c("5/8", "3/8", "3/5", "2/5", "1", "2/3", "1/3", "1", "2/3", "1/3", "1/2", "1/2", "1", "1/2", "1/2", "1/2", "1/2", "1")
Bgraph(function(n) Mn[[n+1]], N=4, USE.COLNAMES = TRUE, label_root = "abcdefgh", 
       labs_edges = labs_edges, 
       cex_vertex = 1, cex_edge = 0.75, 
       path=c(1,0,0,0,0))

# version LaTeX 
colnames(Mn[[1]]) <- c("abcde", "fgh")
colnames(Mn[[2]]) <- c("abc", "de", "fgh")
colnames(Mn[[3]]) <- c("ab", "c", "de", "fg", "h")
colnames(Mn[[4]]) <- c("a", "b", "c", "d", "e", "f", "g", "h")
labs_edges <- c("5/8", "3/8", "3/5", "2/5", "1", "2/3", "1/3", "1", "2/3", "1/3", "1/2", "1/2", "1", "1/2", "1/2", "1/2", "1/2", "1")
Bgraph(function(n) Mn[[n+1]], N=4, USE.COLNAMES = TRUE, label_root = "abcdefgh", 
       labs_edges = labs_edges, 
       cex_vertex = 1, cex_edge = 0.75, 
       path=c(1,0,0,0,0), 
       LaTeX=TRUE)

# compile eps 
setwd("~/Github/myutils/inst/bratteli")
plotCode <- function(){
  Bgraph(function(n) Mn[[n+1]], N=4, USE.COLNAMES = TRUE, label_root = "abcdefgh", 
         labs_edges = labs_edges, 
         cex_vertex = 1, cex_edge = 0.75, 
         path=c(1,0,0,0,0), 
         LaTeX=TRUE)
  return(invisible())
}
plot2tikz(plotCode, format="eps", compile=TRUE, clean=TRUE, overwrite=TRUE, 
          documentDeclaration ="\\documentclass[12pt]{standalone}\n", 
          width=7, height=5)
