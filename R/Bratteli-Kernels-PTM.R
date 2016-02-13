library(gmp)

PTM <- function(n){
  if(n==0) return(t(c(1,1,1,1)))
  return(rbind(c(1,0,0,1),c(1,0,0,1),c(0,1,1,0),c(0,1,1,0)))
}

N <- 2
Mn.fun <- PTM 

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
  Kernels[[k+1]] <- matrix("0", nrow=n, ncol=m, dimnames=list(1:n,1:m))
  for(i in 1:n){
    Kernels[[k+1]][i,][S[[i]]] <- P[[i]]
  }
  dims0 <- dims
}
