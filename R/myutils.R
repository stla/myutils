#' Generates 001, 002, ...
#' 
#' @examples
#' charseq(11)
#' charseq(11,ndigits=3)
#' charseq(11, prefix="graph", suffix=".png")
#' @export
charseq <- function(I, prefix=NULL, suffix=NULL, ndigits=floor(log10(I))+1){
  out <- paste0(prefix, sprintf(paste0("%s", suffix), sprintf(paste0("%0", ndigits, "d"), 1:I)))
  attr(out, "string") <- sprintf("%s%s%s", prefix, paste0("%0", ndigits, "d"), suffix)
  return(out)
}

#' Binary representation
#' 
#' @examples
#' number2binary(1,1)
#' number2binary(3,2)
#' number2binary(7,3)
#' number2binary(15,4)
#' number2binary(15,6)
#' number2binary(15)
#' number2binary(15,3)
#' number2binary(16,5)
#' number2binary(17,5)
#' number2binary(31,5)
#' @export
number2binary <- function(number, noBits=1+floor(log2(max(number,1)))) {
  if(noBits < 1+floor(log2(number))) warning(sprintf("noBits=%s is not enough", noBits))
  binary_vector <- rev(as.numeric(intToBits(number)))
  return( binary_vector[-(1:(length(binary_vector) - noBits))] )
}

