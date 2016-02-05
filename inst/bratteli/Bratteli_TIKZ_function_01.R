setwd("~/Github/myutils/inst/bratteli")

#library(magrittr)
library(diagram)
#library(dplyr) # sert à rien 
library(data.table)

# mettre le RDS dans inst et faire file system 
# faire edges multiples - fait, mais idéalement faudrait calculer le bend en fonction de la distance entre les nodes


#' @param fedgelabels \code{"default"}, \code{NA}, or a function
#' @param bending curvature when there are multiple edges

BgraphTikZ <- function(outfile, fun_Mn, N, fedgelabels="default", ROOTLABEL, LATEXIFY, scale=c(50,50), bending=1){
  Mn <- sapply(0:(N-1), function(n) fun_Mn(n))
  for(i in 1:N){
    if(is.null(colnames(Mn[[i]]))) colnames(Mn[[i]]) <- seq_len(ncol(Mn[[i]]))
  }
  nvertices <- sapply(1:N, function(n) nrow(Mn[[n]])) # number of vertices per level
  left <- c(0L, cumsum(nvertices))
  vertex <- function(n,k){ # n: level ; k: vertex at this level 
    left[n] + k 
  }
  nvertices <- c(nvertices, ncol(Mn[[N]]))
  elpos <- coordinates (nvertices, relsize=1, hor=TRUE) # positions of vertices
  elpos <- setNames(data.table(elpos), c("x", "y"))
  elpos$level <- rep(nvertices, times=nvertices)-1
  # scale 
  elpos[, `:=`(x=scale[1]*x, y=scale[2]*y)]
  # node id's
  elpos[, `:=`(node=myutils::charseq(level+1, LETTERS[level+1])), by="level"]
  elpos$nodelabel <- c(ROOTLABEL, unlist(sapply(1:N, function(n) colnames(Mn[[n]]))))
  if(LATEXIFY) elpos[, nodelabel:=myutils::dollarify()(nodelabel)]
  # code for nodes
  elpos[, code:=sprintf("\\node[VertexStyle](%s) at (%s, %s) {%s};", node, x, y, nodelabel)]
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
  connections <- data.table(connections)
  connections[, id:=paste0(level,from,to), by=1:nrow(connections)] # connections id's
  # edge labels
  if(is.character(fedgelabels) && fedgelabels=="default"){ 
    edgelabels <- TRUE
    connections[, edgelabel:=seq_along(to)-1L, by=node1]
  }
  if(is.function(fedgelabels)){
    edgelabels <- TRUE
    connections[, edgelabel:=fedgelabels(level,from,to)]
  }
  if(is.atomic(fedgelabels) && is.na(fedgelabels)){
    edgelabels <- FALSE
  }
  # curvatures
  fbend <- function(m){
    if(m==1) return(as.numeric(NA))
    bend <- seq(0, 10*bending, length.out = m)*(2*m-2)
    return(bend-mean(bend))
  }
  connections[, bend:=fbend(.N), by="id"]
  if(edgelabels){
    if(LATEXIFY) connections[, edgelabel:=myutils::dollarify()(edgelabel)]
    drawcode <- Vectorize(function(bend){
      if(is.na(bend)) return("\\draw[EdgeStyle](%s) to node[LabelStyle]{%s} (%s);")
      return(paste0(sprintf("\\draw[EdgeStyle, bend left=%s]", bend), "(%s) to node[LabelStyle]{%s} (%s);"))
    })
    connections[, code:=sprintf(drawcode(bend), node1, edgelabel, node2)]
  }else{
    drawcode <- Vectorize(function(bend){
      if(is.na(bend)) return("\\draw[EdgeStyle](%s) to (%s);")
      return(paste0(sprintf("\\draw[EdgeStyle, bend left=%s]", bend), "(%s) to (%s);"))
    })
    connections[, code:=sprintf(drawcode(bend), node1, node2)]
  }
  # write code
  Code <- paste0("\t", c(elpos$code, connections$code), collapse="\n")
  texfile <- sprintf(readRDS("Bratteli_TikZ_template2.RDS"), Code)
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

fun_Mn <- function (n) # Euler
{
  M <- matrix(0L, nrow = n + 1, ncol = n + 2)
  for (i in 1:(n + 1)) {
    M[i, ][c(i, i + 1)] <- c(i, n + 2 - i)
  }
  return(M)
}

LATEXIFY <- TRUE
ROOTLABEL <- "\\varnothing"
outfile <- "tikzcode_test05.tex"
fedgelabels <- function(level, from, to) to-from 

N <- 3

BgraphTikZ(outfile, fun_Mn, N, fedgelabels="default", ROOTLABEL, LATEXIFY, bending=1.5)

# # save template
# x <- paste0(readLines("tikzcode_test04.tex"), collapse="\n")
# saveRDS(x, "Bratteli_TikZ_template2.RDS")
