#' String to letters
#' 
#' @description Separate the letters of a character string
#' 
#' @param string a character string
#' @return a character vector
#' @examples
#' # this returns c("h", "e", "l", "l", "o"):
#' string2letters("hello") 
#' @export
string2letters <- function(string){
  rawToChar(charToRaw(string), multiple = TRUE)
}


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

#' Binary representation (deprecated)
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
  .Deprecated("int2binary")
  if(noBits < 1+floor(log2(number))) warning(sprintf("noBits=%s is not enough", noBits))
  binary_vector <- rev(as.numeric(intToBits(number)))
  return( binary_vector[-(1:(length(binary_vector) - noBits))] )
}

#' Binary representation of integers
#' 
#' @export
#' 
#' @examples
#' int2binary(1,1)
#' int2binary(3,2)
#' int2binary(7,3)
#' int2binary(15,4)
#' int2binary(15,6)
#' int2binary(15)
#' int2binary(15,3)
#' int2binary(16,5)
#' int2binary(17,5)
#' int2binary(31,5)
int2binary <- function(int, noBits=1+floor(log2(max(int,1L)))) {
  if(noBits < 1L+floor(log2(int))) warning(sprintf("noBits=%s is not enough", noBits))
  binary_vector <- rev(as.integer(intToBits(int)))
  return( binary_vector[-(1:(length(binary_vector) - noBits))] )
}

#' Dyadic representation of decimal numbers
#' 
#' @export
num2dyadic <- function(num, nmax=52){ # for 0 <= num < 1
  # 52 = num2dyadic(1-.Machine$double.eps)
  x <- num
  out <- integer(nmax)
  i <- 0L
  j <- 0L
  while(x>0 && i < nmax){
         j <- 1L + floor(-log2(x+.Machine$double.eps)) #floor(-log2(x)-.Machine$double.eps) #
         #i <- i + j
         #if(i <= nmax) out[i] <- 1L
         #x <- 2L^j*x - out[i]
         if(i+j <= nmax){
           i <- i + j
           out[i] <- 1L
           x <- 2L^j*x - 1L
         }else{
           i <- nmax
         }
  }
  if(!all.equal(num, sum(out[1:i]/2^(1:i)))) stop("Incorrect result")
  return(out[1:i])
}

#' Dyadic to decimal
#' 
#' @export
dyadic2num <- function(dyadic){ 
  return(sum(dyadic/2^(seq_along(dyadic))))
}
