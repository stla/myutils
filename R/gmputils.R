#' Convert a bigq matrix to a numeric matrix
#' 
#' @export
bigqmatrix2num <- function(M){
  out <- matrix(numeric(length(M)), nrow=dim(M)[1], ncol=dim(M)[2])
  for(i in 1:nrow(out)){
    out[i,] <- as.numeric(M[i,])
  }
  return(out)
}