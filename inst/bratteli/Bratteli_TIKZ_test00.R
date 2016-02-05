setwd("~/Github/myutils/inst/bratteli")

library(magrittr)
library(diagram)

fun_Mn <- Pascal_Mn <- function(n){
  M <- matrix(0, nrow=n+1, ncol=n+2)
  for(i in 1:(n+1)){
    M[i, ][c(i, i+1)] <- 1
  }
  colnames(M) <- paste0(letters[i+1], 1:ncol(M))
  return(M)
}

fun_Mn <- function (n) 
{
  M <- matrix(0L, nrow = n + 1, ncol = n + 2)
  for (i in 1:(n + 1)) {
    M[i, ][c(i, i + 1)] <- c(i, n + 2 - i)
  }
  return(M)
}

LATEXIFY <- TRUE
ROOTLABEL <- "\\varnothing"

N <- 4
Mn <- sapply(0:(N-1), function(n) fun_Mn(n))
nvertices <- sapply(1:N, function(n) nrow(Mn[[n]])) # number of vertices per level
left <- c(0L, cumsum(nvertices))
vertex <- function(n,k){ # n: level ; k: vertex at this level 
  left[n] + k 
}
nvertices <- c(nvertices, ncol(Mn[[N]]))
elpos <- coordinates (nvertices, relsize=1, hor=TRUE) # positions of vertices
elpos <- setNames(data.frame(elpos), c("x", "y"))
elpos$level <- rep(nvertices, times=nvertices)-1
# scale 
elpos <- transform(elpos, x=100*x, y=100*y)

library(dplyr)
elpos <- tbl_dt(elpos)

f <- function(i){
  myutils::charseq(i+1, LETTERS[i+1])
}
elpos <- elpos[, c(.SD, list(node=f(level))), by="level"]
elpos$nodelabel <- c(ROOTLABEL, unlist(sapply(1:N, function(n) colnames(Mn[[n]]))))

if(LATEXIFY) elpos[, nodelabel:=myutils::dollarify()(nodelabel)]

# code for nodes
elpos[, code:=sprintf("\\node[VertexStyle](%s) at (%s, %s) {%s};", node, x, y, nodelabel)]
print(elpos, n=3)

# code for edges
connections  <-  data.frame(level=integer(), from=integer(), to=integer(), multiplicity=integer(), node1=character(), node2=character(), stringsAsFactors = FALSE)
counter <- 1L
for(n in 0:(N-1)){
  for(i in 1:nrow(Mn[[n+1]])){
    from <- vertex(n+1,i)
    for(k in which(Mn[[n+1]][i,]>0)){ 
      to <- vertex(n+2, k)
      for(m in 1:Mn[[n+1]][i,k]){
        connections[counter,]  <- data.frame(n, i, k, Mn[[n+1]][i,k], elpos[from,]$node, elpos[to,]$node, stringsAsFactors = FALSE)
        counter <- counter+1L
      }
    }
  }
}

# vire ça !!!!
#connections <- tbl_dt(setNames(data.frame(connections), c("level", "from", "to", "node1", "node2")))

connections <- tbl_dt(connections)

# pour les multiple1:
connections[, id:=paste0(level,from,to,collapse="-"), by=1:nrow(connections)]

http://www.inside-r.org/packages/cran/data.table/docs/as.data.table

fbend <- function(m){
  if(m==1) return(as.numeric(NA))
  bend <- seq(0, 10, length.out = m)*m
  return(bend-mean(bend))
}
connections[, bend:=fbend(.N), by="id"]


fedgelabels <- function(level, from, to) to-from # pour pas de labels : ""
connections[, edgelabel:=fedgelabels(level,from,to)]
if(LATEXIFY) connections[, edgelabel:=myutils::dollarify()(edgelabel)]

drawcode <- Vectorize(function(bend){
  if(is.na(bend)) return("\\draw[EdgeStyle](%s) to node[LabelStyle]{%s} (%s);")
  return(paste0(sprintf("\\draw[EdgeStyle, bend right=%s]", bend), "(%s) to node[LabelStyle]{%s} (%s);"))
})

connections[, code:=sprintf(drawcode(bend), node1, edgelabel, node2)]

#connections[, code:=sprintf("\\draw[EdgeStyle](%s) to node[LabelStyle]{%s} (%s);", node1, edgelabel, node2)]
  


# write code
#writeLines(c(elpos$code, connections$code), "tikzcode_test00.txt") 

Code <- paste0("\t", c(elpos$code, connections$code), collapse="\n")
texfile <- sprintf(readRDS("Bratteli_TikZ_template.RDS"), Code)
writeLines(texfile, "tikzcode_test03.tex")

