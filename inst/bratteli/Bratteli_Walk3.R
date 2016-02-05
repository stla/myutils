setwd("~/Github/myutils/inst/bratteli")
source('~/Github/myutils/inst/bratteli/Bratteli_Dims_and_Kernels.R')

# génération des matrices d'incidence de l'arbre partant d'un vertex

fun_Mn <- Pascal_Mn <- function(n){
  M <- matrix(0, nrow=n+1, ncol=n+2)
  for(i in 1:(n+1)){
    M[i, ][c(i, i+1)] <- 1
  }
  colnames(M) <- paste0(letters[i+1], 1:ncol(M))
  return(M)
}


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


N <- 4 # level 
v <- 3 # vertex
M_N <- fun_Mn(N)
Dims <- VerticesDims(fun_Mn, N+1)
colnames(M_N) <- sapply(Dims[[N+1]], function(i) paste0(letters[1:i], collapse=""))
rownames(M_N) <- Dims[[N]]

QQ <- vector("list", N)

QQ[[1]] <- Qv(M_N[,v], colnames(M_N)[v], rownames(M_N)) # doit donner Q0
i1 <- attr(QQ[[1]], "i0")

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
      Q <-  Qv(Mnext[,i0[k]], colnames(Q1)[k], rownames(Mnext)) 
      Qnext[[counter]] <- Q
      counter <- counter+1
      i1 <- c(i1, attr(Q, "i0"))
    }
  }
  QQ[[i+1]] <- myutils::blockdiag_list(Qnext)
}


fun_Mn <- function(n) QQ[[n+1]]

# => tikz 
  # => pour les probas sur les labels, suffit de diviser les nchar ! 

Bwalk <- function(fun_Mn, N, v){
  M_N <- fun_Mn(N)
  Dims <- Bdims(fun_Mn, N+1)
  colnames(M_N) <- sapply(Dims[[N+1]], function(i) paste0(letters[1:i], collapse=""))
  rownames(M_N) <- Dims[[N]]
  QQ <- vector("list", N)
  QQ[[1]] <- Qv(M_N[,v], colnames(M_N)[v], rownames(M_N)) # doit donner Q0
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
        Q <-  Qv(Mnext[,i0[k]], colnames(Q1)[k], rownames(Mnext)) 
        Qnext[[counter]] <- Q
        counter <- counter+1
        i1 <- c(i1, attr(Q, "i0"))
      }
    }
    QQ[[i+1]] <- myutils::blockdiag_list(Qnext)
  }
  return(QQ)
}
