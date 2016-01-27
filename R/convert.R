#' Convert (e)ps to png/gif
#' 
#' 
#' @export
ps2img <- function(psfile, format="gif"){
  pngfile <- paste0(knitr:::sans_ext(psfile), ".", format)
  command <- ifelse(format=="gif", sprintf("convert -trim %s %s", psfile, pngfile),
                    sprintf("gs -r300 -dEPSCrop -dTextAlphaBits=4 -sDEVICE=png16m -sOutputFile=%s -dBATCH -dNOPAUSE %s", pngfile, psfile))
  system(command)
  return(invisible())
}