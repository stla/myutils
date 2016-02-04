setwd("~/Github/myutils/inst/bratteli")

#library(magrittr)
library(diagram)
library(dplyr) # sert à rien 
library(data.table)

# mettre le RDS dans inst et faire file system 
# faire edges multiples 
# virer les data.table qui s'affichent 

#' @param fedgelabels \code{"default"}, \code{NA}, or a function

BgraphTikZ <- function(outfile, fun_Mn, N, fedgelabels="default", ROOTLABEL, LATEXIFY, scale=c(100,100)){
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
  elpos <- transform(elpos, x=scale[1]*x, y=scale[2]*y)
  # node id's
  elpos <- tbl_dt(elpos) # rq : dplyr inutile
  elpos <- elpos[, c(.SD, list(node=myutils::charseq(level+1, LETTERS[level+1]))), by="level"]
  elpos$nodelabel <- c(ROOTLABEL, unlist(sapply(1:N, function(n) colnames(Mn[[n]]))))
  if(LATEXIFY) elpos[, nodelabel:=myutils::dollarify()(nodelabel)]
  # code for nodes
  elpos[, code:=sprintf("\\node[VertexStyle](%s) at (%s, %s) {%s};", node, x, y, nodelabel)]
  print(elpos, n=3)
  # code for edges
  connections  <-  data.frame(level=integer(), from=integer(), to=integer(), node1=character(), node2=character(), stringsAsFactors = FALSE)
  counter <- 1L
  for(n in 0:(N-1)){
    for(i in 1:nrow(Mn[[n+1]])){
      from <- vertex(n+1,i)
      for(k in which(Mn[[n+1]][i,]>0)){
        to <- vertex(n+2, k)
        connections[counter,]  <- data.frame(n, i, k, elpos[from,]$node, elpos[to,]$node, stringsAsFactors = FALSE)
        counter <- counter+1L
      }
    }
  }
  connections <- tbl_dt(setNames(data.frame(connections), c("level", "from", "to", "node1", "node2")))
  if(is.character(fedgelabels) && fedgelabels=="default"){ 
    connections[, edgelabel:=seq_along(to)-1L, by=node1]
    if(LATEXIFY) connections[, edgelabel:=myutils::dollarify()(edgelabel)]
    connections[, code:=sprintf("\\draw[EdgeStyle](%s) to node[LabelStyle]{%s} (%s);", node1, edgelabel, node2)]
  }
  if(is.function(fedgelabels)){
    connections[, edgelabel:=fedgelabels(level,from,to)]
    if(LATEXIFY) connections[, edgelabel:=myutils::dollarify()(edgelabel)]
    connections[, code:=sprintf("\\draw[EdgeStyle](%s) to node[LabelStyle]{%s} (%s);", node1, edgelabel, node2)]
  }
  if(is.atomic(fedgelabels) && is.na(fedgelabels)){
    #connections[, edgelabel:=""]
    connections[, code:=sprintf("\\draw[EdgeStyle](%s) to (%s);", node1, node2)]
  }
  # write code
  Code <- paste0("\t", c(elpos$code, connections$code), collapse="\n")
  texfile <- sprintf(readRDS("Bratteli_TikZ_template.RDS"), Code)
  writeLines(texfile, outfile)
  return(invisible())
}


fun_Mn <- Pascal_Mn <- function(n){
  M <- matrix(0, nrow=n+1, ncol=n+2)
  for(i in 1:(n+1)){
    M[i, ][c(i, i+1)] <- 1
  }
  colnames(M) <- paste0(letters[i+1], 1:ncol(M))
  return(M)
}

LATEXIFY <- TRUE
ROOTLABEL <- "\\varnothing"
scale <- c(100, 100)
outfile <- "tikzcode_test02.tex"
fedgelabels <- function(level, from, to) to-from 

N <- 3

BgraphTikZ(outfile, fun_Mn, N, fedgelabels=NA, ROOTLABEL, LATEXIFY, scale=c(100,100))

