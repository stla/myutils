#' Central kernels on a Bratteli graph
#' 
#' @export
#' @import gmp
#' 
#' @examples
#' library(gmp)
#' Euler <- function(n)
#' {
#'   M <- matrix(0L, nrow = n + 1, ncol = n + 2)
#'   for (i in 1:(n + 1)) {
#'     M[i, ][c(i, i + 1)] <- c(i, n + 2 - i)
#'   }
#'   return(M)
#' }
#' Bkernels(Euler, 3)
#' 
Bkernels <- function(Mn.fun, N, class=c("character", "bigq")){
  if(is.null(sessionInfo()$otherPkgs) || !"gmp" %in% names(sessionInfo()$otherPkgs)) stop("load the gmp package")
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
    S <- lapply(1:ncol(M), function(i) which(M[,i]!=0)) 
    dims <- as.vector(dims0%*%M) 
    P <- lapply(1:n, function(i){
      as.character(dims0[S[[i]]]*M[S[[i]],i]/dims[i])
    })
    #Kernels[[k+1]] <- matrix("0", nrow=n, ncol=m, dimnames=list(1:n,1:m))
    Kernels[[k+1]] <- matrix("0", nrow=n, ncol=m, dimnames=dimnames(t(M)))
    for(i in 1:n){
      Kernels[[k+1]][i,][S[[i]]] <- P[[i]]
    }
    dims0 <- dims
  }
  if(match.arg(class)=="bigq") Kernels <- lapply(Kernels, as.bigq)
  return(Kernels)
}

#' Vertices dimensions of a Bratteli graph 
#' 
#' @export
#' 
#' @examples
#' Euler <- function(n)
#' {
#'   M <- matrix(0L, nrow = n + 1, ncol = n + 2)
#'   for (i in 1:(n + 1)) {
#'     M[i, ][c(i, i + 1)] <- c(i, n + 2 - i)
#'   }
#'   return(M)
#' }
#' Bdims(Euler, 4)
#' 
Bdims <- function(Mn.fun, N){
  Dims <- vector("list", N) 
  # initialization 
  k <- 0
  M <- Mn.fun(k)
  if(nrow(M) != 1) stop("M0 must have only one row")
  Dims[[k+1]] <- dims0 <- as.vector(M)
  if(N>1){
    for(k in 1:(N-1)){
      Dims[[k+1]] <- dims0 <- as.vector(dims0 %*% Mn.fun(k)) 
    }
  }
  return(Dims)
}

#' Intrisic metrics on a Bratteli graph 
#' 
#' @export
#' @import kantorovich
#' @examples
#' library(gmp)
#' Euler <- function(n)
#' {
#'   M <- matrix(0L, nrow = n + 1, ncol = n + 2)
#'   for (i in 1:(n + 1)) {
#'     M[i, ][c(i, i + 1)] <- c(i, n + 2 - i)
#'   }
#'   return(M)
#' }
#' Bmetrics(Euler, 3)
Bmetrics <- function(Mn.fun, N, solver=c("rcdd", "glpk")){
  ckernels <- Bkernels(Mn.fun, N) 
  solver <- match.arg(solver)
  if(solver=="rcdd"){
    RHO <- lapply(ckernels, function(kernel) matrix("", nrow=nrow(kernel), ncol=nrow(kernel)))
  }else{
    RHO <- lapply(ckernels, function(kernel) matrix(0, nrow=nrow(kernel), ncol=nrow(kernel)))  
  }
  RHO[[1]] <- (diag(nrow(ckernels[[1]])) + 1) %% 2
  n <- length(ckernels)-1
  if(solver=="rcdd"){
    kanto <- function(mu, nu, dist){
      as.character(kantorovich(as.bigq(mu), as.bigq(nu), dist=unname(dist)))
    }
  }else{
    kanto <- function(mu, nu, dist){
      kantorovich_glpk(as.numeric(as.bigq((mu))), as.numeric(as.bigq(nu)), dist=myutils::bigqmatrix2num(as.bigq(dist)))
    }
  }
  for(k in 1:n){
    diag(RHO[[k+1]]) <- ifelse(solver=="rcdd", "0", 0)
    K <- nrow(RHO[[k+1]])
    kernel <- ckernels[[k+1]]
    for(i in 1:(K-1)){
      for(j in (i+1):K){
        # RHO[[k+1]][i,j] <- RHO[[k+1]][j,i] <- 
        #   as.character(kantorovich(as.bigq(kernel[i,]), as.bigq(kernel[j,]), dist = unname(RHO[[k]])))
        RHO[[k+1]][i,j] <- RHO[[k+1]][j,i] <- kanto(kernel[i,], kernel[j,], RHO[[k]])
      }
    }
    dimnames(RHO[[k+1]]) <- list(colnames(Mn.fun(k)), colnames(Mn.fun(k)))
  }
  return(RHO)
}