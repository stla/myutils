setwd("~/Github/myutils/inst/bratteli")

library(gmp)
centralKernels <- function(Mn.fun, N){
  Kernels <- vector("list", N) 
  # initialization 
  k <- 0
  M <- Mn.fun(k)
  m <- nrow(M); n <- ncol(M)
  if(m != 1) stop("M0 must have only one row")
  dims0 <-  as.vector(as.bigz(M))
  Kernels[[k+1]] <- matrix(as.character(dims0), dimnames=list(1:n, 1:m))
  for(k in 1:(N-1)){
    M <- Mn.fun(k)
    m <- nrow(M); n <- ncol(M)
    S <- apply(M, 2, function(x) which(x!=0)) 
    dims <- as.vector(dims0%*%M) 
    P <- lapply(1:n, function(i){
      as.character(dims0[S[[i]]]*M[S[[i]],i]/dims[i])
    })
    Kernels[[k+1]] <- matrix("0", nrow=n, ncol=m, dimnames=list(1:n,1:m))
    for(i in 1:n){
      Kernels[[k+1]][i,][S[[i]]] <- P[[i]]
    }
    dims0 <- dims
  }
  return(Kernels)
}


VerticesDims <- function(Mn.fun, N){
  Dims <- vector("list", N) 
  # initialization 
  k <- 0
  M <- Mn.fun(k)
  if(nrow(M) != 1) stop("M0 must have only one row")
  Dims[[k+1]] <- dims0 <- as.vector(M)
  for(k in 1:(N-1)){
    Dims[[k+1]] <- dims0 <- as.vector(dims0 %*% Mn.fun(k)) 
  }
  return(Dims)
}

fun_Mn <- Pascal_Mn <- function(n){
  M <- matrix(0, nrow=n+1, ncol=n+2)
  for(i in 1:(n+1)){
    M[i, ][c(i, i+1)] <- 1
  }
  colnames(M) <- paste0(letters[i+1], 1:ncol(M))
  return(M)
}

centralKernels(fun_Mn, 3)
VerticesDims(fun_Mn, 3)

fun_Mn <- function (n) # Euler
{
  M <- matrix(0L, nrow = n + 1, ncol = n + 2)
  for (i in 1:(n + 1)) {
    M[i, ][c(i, i + 1)] <- c(i, n + 2 - i)
  }
  return(M)
}

centralKernels(fun_Mn, 3)

VerticesDims(fun_Mn, 3)
