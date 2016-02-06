setwd("~/Github/myutils/inst/bratteli")
source('~/Github/myutils/inst/bratteli/old/Bratteli_Dims_and_Kernels.R')

# génération des matrices d'incidence de l'arbre partant d'un vertex

fun_Mn <- Pascal_Mn <- function(n){
  M <- matrix(0, nrow=n+1, ncol=n+2)
  for(i in 1:(n+1)){
    M[i, ][c(i, i+1)] <- 1
  }
  colnames(M) <- paste0(letters[i+1], 1:ncol(M))
  return(M)
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

N <- 3; v <- 3


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
