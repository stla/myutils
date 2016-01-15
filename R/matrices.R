#' Block diagonal matrix
#' 
#' @param M1 top-left block
#' @param M2 bottom-right block
#' 
#' @examples
#' M1 <- diag(2); M2 <- matrix(2, nrow=3, ncol=3)
#' blockdiag(M1,M2)
#' 
#' @export
blockdiag <- function(M1, M2){
  return(rbind(cbind(M1, matrix(0, nrow=nrow(M1), ncol=ncol(M2))),
        cbind(matrix(0, nrow=nrow(M2), ncol=ncol(M1)), M2)))
}