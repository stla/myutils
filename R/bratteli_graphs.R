#' Draw a Bratteli graph in R
#' 
#' @examples 
#' Pascal_Mn <- function(n){
#'  M <- matrix(0, nrow=n+1, ncol=n+2)
#'  for(i in 1:(n+1)){
#'    M[i, ][c(i, i+1)] <- 1
#'  }
#'  return(M)
#' }
#' Bgraph(Pascal_Mn, N=3, path=c(1,0,1), first_vertex=1, hor=FALSE)
#' 
#' @import diagram
#' @export
Bgraph <- function(fun_Mn, N, title=NA, 
                   path=NULL, col_path="blue", lwd_path=4, labels_path=FALSE, 
                   labels_vertex=TRUE, USE.COLNAMES=FALSE, first_vertex=0, label_root="ø", only_end=FALSE, cex_vertex=1.5, 
                   voffset_labels=0, hoffset_labels=function(n,i) 0,   
                   labels_edges=TRUE, labs_edges=NULL, cex_edge=1.1, col_labels_edge="black", 
                   col_edge="gray", lwd_edge=2, 
                   ellipse_vertex=FALSE, ellipse_edge=FALSE, LaTeX=FALSE, 
                   points_vertex=FALSE, pch_vertex=19, cex_pch=1, ...){
  Mn <- sapply(0:(N-1), function(n) fun_Mn(n))
  nvertices <- sapply(1:N, function(n) nrow(Mn[[n]])) # number of vertices per level
  elpos <- coordinates (c(nvertices, ncol(Mn[[N]])), ...) # positions of vertices
  left <- cumsum(nvertices)
  vertex <- function(n,k){ # n: level ; k: vertex at this level 
    left[n] + k 
  }
  # 
  l <- length(path)
  path <- c(path, rep(0,N+1-l))
  if(length(col_path)==1) col_path <- rep(col_path, length(path))
  path <- path +1  # NA useless but..
  vpath <- path[1]
  for(k in 2:N){
    v <- vpath[k-1] 
    vpath[k] <- which(Mn[[k]][v,]>0)[path[k]]
  }
  path <- vpath 
  # construct connections matrix
  fromto <- NULL
  multiplicity <- NULL
  lcol <- lty <- lwd <- NULL
  # connections from level n=0
  current <- 1 
  for(i in which(Mn[[1]][1,]>0)){
    goto <- vertex(1, i)
    fromto <- rbind(fromto, c(current,goto))
    multiplicity <- c(multiplicity, Mn[[1]][1,i])
    lcol <- rbind(lcol, 
                  ifelse(i==path[1], col_path[1], "gray")
    )
    lty <- rbind(lty, 
                 ifelse(i==path[1], "solid", "solid")
    )
    lwd <- rbind(lwd, 
                 ifelse(i==path[1], lwd_path, lwd_edge)
    )
  }
  # connections from level n
  for(n in 1:(N-1)){
    for(i in 1:nrow(Mn[[n+1]])){
      current <- vertex(n,i)
      path_done <- FALSE
      for(k in which(Mn[[n+1]][i,]>0)){
        goto <- vertex(n+1, k)
        fromto <- rbind(fromto, c(current,goto))
        multiplicity <- c(multiplicity, Mn[[n+1]][i,k])
        s <- which.max(Mn[[n+1]][i,]>0)-1
        lcol <- rbind(lcol, ifelse(all(c(i,k)==path[n:(n+1)]), col_path[n+1], col_edge)
        )
        lty <- rbind(lty, ifelse(all(c(i,k)==path[n:(n+1)]), "solid", "solid")
        )
        lwd <- rbind(lwd, ifelse(all(c(i,k)==path[n:(n+1)]), lwd_path, lwd_edge)
        )
      }
    }
  }
  nr <- nrow(fromto)
  if(labels_edges){
    if(is.null(labs_edges)) labs_edges <- unlist( by(fromto[,1], fromto[,1], FUN=function(x) 1:length(x)) ) - 1
    if(LaTeX) labs_edges <- paste0("$", labs_edges, "$")
  }
  arrpos <- matrix(ncol = 2, nrow = nr)
  # START PLOT 
  openplotmat(main=title)
  for (i in 1:nr){ # draw and store the arrows
    if(multiplicity[i]==1){
      arrpos[i, ] <- straightarrow (to = elpos[fromto[i, 2], ],
                                    from = elpos[fromto[i, 1], ],
                                    arr.pos = 0.5, arr.length = 0, 
                                    lcol=lcol[i], lty=lty[i], lwd=lwd[i])
    }
    if(multiplicity[i]==2){
      curvedarrow (to = elpos[fromto[i, 2], ],
                   from = elpos[fromto[i, 1], ],
                   arr.pos = 0.5, arr.length = 0, 
                   lcol=lcol[i], lty=lty[i], lwd=lwd[i],
                   curve=-0.05)
      arrpos[i, ] <- curvedarrow (to = elpos[fromto[i, 2], ],
                                  from = elpos[fromto[i, 1], ],
                                  arr.pos = 0.5, arr.length =0, 
                                  lcol=lcol[i], lty=lty[i], lwd=lwd[i],
                                  curve=0.05)
    }
    # labels on edges 
    if(labels_edges && !labels_path){ 
      mid <- arrpos[i, ]
      if(ellipse_edge){
        textellipse(mid, 0.02, lab=labs_edges[i], cex=cex_edge, lcol="black", col=col_labels_edge, shadow.size=0) 
      }else{
        textempty(mid+c(0,0), lab=labs_edges[i], cex=cex_edge, col=col_labels_edge)
      }
    }
    # labels on the blue path
    if(labels_path){
      if(lcol[i]==col_path){
        mid <- arrpos[i, ]
        if(ellipse_edge){
          textellipse(mid, 0.02, lab=labs_edges[i], cex=cex_edge, lcol="black", shadow.size=0) 
        }else{
          textempty(mid+c(0,0), lab=labs_edges[i], cex=cex_edge)
        }
      }
    }
  }
  # labels on vertices
  if(labels_vertex){
    # root label 
    if(LaTeX) label_root <- paste0("$", label_root, "$")
    textellipse(elpos[1,], 0.01, 0.01, lab = label_root, box.col="white", shadow.size=0, lcol="white", cex=cex_vertex) 
    # vertices labels
    start <- ifelse(only_end, N, 1)
    for(n in start:N){ 
      for(i in 1:ncol(Mn[[n]])){
        lab <- as.character(ifelse(USE.COLNAMES, colnames(Mn[[n]])[i] , i-1+first_vertex))
        # if(LaTeX && !USE.COLNAMES) lab <- paste0("$", lab, "$")
        if(LaTeX) lab <- paste0("$", lab, "$")
        if(ellipse_vertex){
          textellipse(elpos[vertex(n,i),], 0.02, 0.02, lab=lab, box.col="white", shadow.size=0, cex=cex_vertex)          
        }else{
          textempty(elpos[vertex(n,i),] + c(hoffset_labels(n,i), voffset_labels), lab = lab, cex=cex_vertex)  
        }
      }
    }
  }
  # points on vertices
  if(points_vertex){
    # root label 
    textempty(elpos[1,],  lab = label_root, cex=cex_vertex) 
    # vertices labels
    start <- ifelse(only_end, N, 1)
    for(n in start:N){ 
      for(i in 1:ncol(Mn[[n]])){
          points(elpos[vertex(n,i),1], elpos[vertex(n,i),2], pch=pch_vertex, cex=cex_pch)  
      }
    }
  }
}


#' Generate TikZ code of a Bratteli graph
#' 
#' @export
#' @param edgelabels \code{"default"}, \code{"default_letters"}, \code{"order"}, \code{"kernels"}, \code{NA}, or a VECTORIZED function
#' @param vertexlabels \code{"colnames"} (default) to use the column names of the matrices, \code{"dims"} to use the dimensions of the vertices, or a VECTORIZED(?) function
#' @param bending curvature when there are multiple edges
#' @param northsouth node connections
#' @examples 
#' Pascal_Mn <- function(n){
#'  M <- matrix(0, nrow=n+1, ncol=n+2)
#'  for(i in 1:(n+1)){
#'    M[i, ][c(i, i+1)] <- 1
#'  }
#'  return(M)
#' }
#' BgraphTikZ("/tmp/PascalGraph.tex", Pascal_Mn, 3)
#' 
BgraphTikZ <- function(outfile, fun_Mn, N, 
                       edgelabels="default", 
                       vertexlabels="colnames", 
                       ROOTLABEL="\\varnothing", LATEXIFY=TRUE, 
                       packages=NULL, 
                       scale=c(50,50), bending=1, 
                       hor=FALSE, mirror=FALSE, 
                       northsouth=FALSE){
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
  if(is.character(vertexlabels)){
    if(vertexlabels=="colnames") fvertexlabels <- function(n) colnames(Mn[[n]])
    if(vertexlabels=="dims"){
      dims <- myutils::Bdims(fun_Mn, N)
      fvertexlabels <- function(n) dims[[n]]
    }
  }
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
  # edge labels
  if(is.character(edgelabels)){ 
    labels_on_edge <- TRUE
    if(edgelabels=="default") connections[, edgelabel:=seq_along(to)-1L, by=node1]
    if(edgelabels=="default_letters") connections[, edgelabel:=letters[seq_along(to)], by=node1]
    if(edgelabels=="order") connections[, edgelabel:=seq_len(.N)-1L, by=node2]
    if(edgelabels=="kernels"){
      if(!is.element("nicefrac", packages)) packages <- c(packages, "nicefrac")
      require(gmp, quietly = TRUE)
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
  if(is.function(edgelabels)){
    labels_on_edge <- TRUE
    connections[, edgelabel:=edgelabels(level,from,to,mindex)]
  }
  if(is.atomic(edgelabels) && is.na(edgelabels)){
    labels_on_edge <- FALSE
  }
  # curvatures
  fbend <- function(m){
    if(m==1) return(as.numeric(NA))
    bend <- seq(0, 10*bending, length.out = m)*(2*m-2)
    return(bend-mean(bend))
  }
  connections[, bend:=fbend(.N), by="id"]
  if(labels_on_edge){
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
  #return(connections)
  return(invisible())
}

#' Incidences matrices for a walk on a Bratteli graph - walk on words
#' 
#' @export
#' 
#' @examples 
#' Pascal <- function(n){
#'  M <- matrix(0, nrow=n+1, ncol=n+2)
#'  for(i in 1:(n+1)){
#'    M[i, ][c(i, i+1)] <- 1
#'  }
#'  return(M)
#' }
#' Bwalk(Pascal, 4, 3)
#' 
Bwalk <- function(fun_Mn, N, v){
  .Deprecated("Bwalk_powers")
  M_N <- fun_Mn(N)
  Dims <- Bdims(fun_Mn, N+1)
  colnames(M_N) <- sapply(Dims[[N+1]], function(i) paste0(letters[1:i], collapse=""))
  rownames(M_N) <- Dims[[N]]
  QQ <- vector("list", N)
  QQ[[1]] <- myutils:::Qv(M_N[,v], colnames(M_N)[v], rownames(M_N)) # doit donner Q0
  i1 <- attr(QQ[[1]], "i0")
  attr(QQ[[1]], "i0") <- NULL
  for(i in 1:(N-1)){
    Mnext <- fun_Mn(N-i)
    rownames(Mnext) <- Dims[[N-i]]
    i0 <- i1
    i1 <- NULL
    counter <- 1
    Q1 <- QQ[[i]]
    Qnext <- list()
    for(j in 1:nrow(Q1)){
      for(k in which(Q1[j,]>0)){
        Q <-  myutils:::Qv(Mnext[,i0[k]], colnames(Q1)[k], rownames(Mnext)) 
        Qnext[[counter]] <- Q
        counter <- counter+1
        i1 <- c(i1, attr(Q, "i0"))
      }
    }
    QQ[[i+1]] <- myutils::blockdiag_list(Qnext)
  }
  return(QQ)
}

#' internal function for Bwalk
Qv <- function(column, word, dims){
  names(column) <- dims
  i0 <- which(column>0)
  Q0 <- t(column[i0])
  rownames(Q0) <- word
  subwords <- character(ncol(Q0))
  begin <- 1L
  for(i in 1:ncol(Q0)){
    end <- begin+as.integer(colnames(Q0)[i])-1L
    subwords[i] <- stringr::str_sub(word, begin, end)
    begin <- end+1L
  }
  colnames(Q0) <- subwords
  attr(Q0, "i0") <- unname(i0)
  return(Q0)
}

#' internal function for Bwalk_powers
Qv_powers <- function(column, power, dims){
  names(column) <- dims
  i0 <- which(column>0)
  Q0 <- t(column[i0])
  rownames(Q0) <- power
  powers <- integer(ncol(Q0))
  begin <- 0L
  for(i in 1:ncol(Q0)){
    end <- begin+as.integer(colnames(Q0)[i])-1L
    powers[i] <- begin # stringr::str_sub(word, begin, end)
    begin <- end+1L
  }
  colnames(Q0) <- powers + power
  attr(Q0, "i0") <- unname(i0)
  return(Q0)
}

#' Incidences matrices for a walk on a Bratteli graph - walk on powers
#' 
#' @export
#' 
#' @examples 
#' Pascal <- function(n){
#'  M <- matrix(0, nrow=n+1, ncol=n+2)
#'  for(i in 1:(n+1)){
#'    M[i, ][c(i, i+1)] <- 1
#'  }
#'  return(M)
#' }
#' Bwalk_powers(Pascal, 4, 3)
#' 
Bwalk_powers <- function(fun_Mn, N, v, labels=c("powers", "words")){
  M_N <- fun_Mn(N)
  Dims <- myutils::Bdims(fun_Mn, N+1)
  colnames(M_N) <- rep(0, ncol(M_N)) # sapply(Dims[[N+1]], function(i) paste0(letters[1:i], collapse=""))
  dims <- Dims[[N]]
  QQ <- vector("list", N)
  QQ[[1]] <- myutils:::Qv_powers(M_N[,v], as.integer(colnames(M_N)[v]), dims) 
  i1 <- attr(QQ[[1]], "i0")
  attr(QQ[[1]], "dims") <- list(rows=Dims[[N+1]][v], cols=dims[i1])
  attr(QQ[[1]], "i0") <- NULL
  for(i in 1:(N-1)){
    Mnext <- fun_Mn(N-i)
    dims <-  Dims[[N-i]] #rownames(Mnext) <- Dims[[N-i]]
    i0 <- i1
    i1 <- NULL
    counter <- 1
    Q1 <- QQ[[i]]
    Qnext <- list()
    for(j in 1:nrow(Q1)){
      for(k in which(Q1[j,]>0)){
        Q <-  myutils:::Qv_powers(Mnext[,i0[k]], as.integer(colnames(Q1)[k]), dims) 
        Qnext[[counter]] <- Q
        counter <- counter+1
        i1 <- c(i1, attr(Q, "i0"))
      }
    }
    if(length(Qnext)==1L){
      QQ[[i+1]] <- Qnext[[1]]
      attr(QQ[[i+1]], "i0") <- NULL
    }else{
      QQ[[i+1]] <- myutils::blockdiag_list(Qnext) 
    }
    attr(QQ[[i+1]], "dims") <- list(rows=attr(QQ[[i]], "dims")$cols, cols=dims[i1])
  }
  # Kernels
  # library(gmp)
  Kernels <- lapply(QQ, function(P){
    Pcopy <- gmp::as.bigq(matrix(0, nrow=nrow(P), ncol=ncol(P)))
    dims <- attr(P, "dims")
    for(i in 1:nrow(P)){
      j <- which(P[i,]!=0)
      Pcopy[i,j] <- gmp::as.bigq(dims$cols[j], dims$rows[i])
    }
    return(Pcopy)
  })
  # word labels 
  labels <- match.arg(labels)
  if(labels=="words"){
    L <- attr(QQ[[1]], "dims")$rows
    if(L <= 52){ 
      word0 <- c(letters,LETTERS)[1:L]
      f <- Vectorize(function(first, length){
        paste0(word0[(first+1):(first+length)], collapse="")
      })
      for(n in 1:N){
        dims <- attr(QQ[[n]], "dims")
        rownames(QQ[[n]]) <- f(as.integer(rownames(QQ[[n]])), dims$rows)
        colnames(QQ[[n]]) <- f(as.integer(colnames(QQ[[n]])), dims$cols)
        attr(QQ[[n]], "dims") <- NULL
      }
    }else{
      cat("Words too long")
    }
  }
  return(list(Mn=QQ, Pn=Kernels))
}