setwd("~/Github/myutils/inst/bratteli")

#library(magrittr)
library(diagram)
library(data.table)

outfile <- "BgraphChacon00.tex"
fun_Mn <- Chacon <- function(n){
  if(n==0) return(t(c(1L,1L)))
  return(t(matrix(c(3L, 1L, 0L, 1L), byrow=TRUE, nrow=2)))
}
N <- 4
scale=c(50,50)
bending=1
fvertexlabels=NULL 
ROOTLABEL="\\varnothing"; LATEXIFY=TRUE 
packages=NULL 
scale=c(50,50); bending=2 
hor=FALSE; mirror=FALSE
northsouth=FALSE

fedgelabels <- Vectorize(function(n, from, to, mindex){
  if(n==0) return(".")
  if(from==2) return(ifelse(to==1, "2", "0")) 
  if(mindex %in% c(1,2)) return(mindex-1) else return(3)
})

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
  elpos <- coordinates (nvertices, relsize=1, hor=!hor) # positions of vertices
  elpos <- setNames(data.table(elpos), c("x", "y"))
  if(mirror){
    if(!hor) elpos[, y:=max(y)-y] else elpos[, x:=max(x)-x]
  }
  elpos$level <-  rep(seq_along(nvertices), times=nvertices)-1L
  # scale 
  elpos[, `:=`(x=scale[1]*x, y=scale[2]*y)]
  # node id's
  elpos[, `:=`(node=myutils::charseq(.N, LETTERS[level[1]+1])), by="level"]
  #elpos[, `:=`(node=myutils::charseq(level+1, LETTERS[level+1])), by="level"]
  if(is.null(fvertexlabels)) fvertexlabels <- function(n) colnames(Mn[[n]])
  
  ### xxx
  if(is.character(fvertexlabels) && fvertexlabels=="dims"){
    dims <- myutils::Bdims(fun_Mn, N)
    fvertexlabels <- function(n) dims[[n]]
  }
  ### xxx
  
  elpos$nodelabel <- c(ROOTLABEL, unlist(sapply(1:N, function(n) fvertexlabels(n))))
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
  
  ## indice multiplicité
  connections[, `:=`(mindex=seq_len(.N)), by="id"]
  ##
  
  # edge labels
  

  ### xxx
  
  if(is.character(fedgelabels)){ 
    edgelabels <- TRUE
    if(fedgelabels=="default") connections[, edgelabel:=seq_along(to)-1L, by=node1]
    if(fedgelabels=="kernels"){
      ckernels <- myutils::Bkernels(fun_Mn, N, class="bigq")
      ckernels_numer <- lapply(ckernels, function(x) as.character(numerator(x)))
      ckernels_denom <- lapply(ckernels, function(x) as.character(denominator(x)))
      f <- Vectorize(function(level, from, to){
        if(ckernels_denom[[level+1]][to,from]=="1") return("1")
        sprintf("\\nicefrac{%s}{%s}", ckernels_numer[[level+1]][to,from], ckernels_denom[[level+1]][to,from])
      })
      connections[, edgelabel:=f(level,from,to)]
    }
  }
  if(is.function(fedgelabels)){
    edgelabels <- TRUE
    connections[, edgelabel:=fedgelabels(level,from,to,mindex)]
  }
  if(is.atomic(fedgelabels) && is.na(fedgelabels)){
    edgelabels <- FALSE
  }
  
  ### xxx 
  connections[, edgelabel:=seq_len(.N)-1L, by=node2]
  ### xxxx
  
  
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
      if(is.na(bend)) return(ifelse(northsouth, "\\draw[EdgeStyle](%s.south) to node[EdgeLabelStyle]{%s} (%s.north);", "\\draw[EdgeStyle](%s) to node[EdgeLabelStyle]{%s} (%s);"))
      return(paste0(sprintf("\\draw[EdgeStyle, bend left=%s]", bend), ifelse(northsouth, "(%s.south) to node[EdgeLabelStyle]{%s} (%s.north);", "(%s) to node[EdgeLabelStyle]{%s} (%s);")))
    })
    connections[, code:=sprintf(drawcode(bend), node1, edgelabel, node2)]
  }else{
    drawcode <- Vectorize(function(bend){
      if(is.na(bend)) return(ifelse(northsouth, "\\draw[EdgeStyle](%s.south) to (%s.north);", "\\draw[EdgeStyle](%s) to (%s);"))
      return(paste0(sprintf("\\draw[EdgeStyle, bend left=%s]", bend), ifelse(northsouth, "(%s.south) to (%s.north);", "(%s) to (%s);")))
    })
    connections[, code:=sprintf(drawcode(bend), node1, node2)]
  }
  # TikZ code
  Code <- paste0("\t", c(elpos$code, connections$code), collapse="\n")
  # add packages
  if(!is.null(packages)){
    packages <- sapply(packages, function(x) sprintf("\\usepackage{%s}\n", x))
  }else{
    packages <- ""
  }
  # write code to template
  template <- system.file("templates", "template_BratteliTikZ2.RDS", package="myutils")
  texfile <- sprintf(readRDS(template), packages, Code)
  writeLines(texfile, outfile)
  
  



  
# # save template
# x <- paste0(readLines("tikzcode_test04.tex"), collapse="\n")
# saveRDS(x, "template_BratteliTikZ2.RDS")
