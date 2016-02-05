#' Convert (e)ps to png/gif
#' 
#' 
#' @export
pspdf2img <- function(infile, format="png", moreopts="", ps2eps=FALSE){
  outfile <- paste0(knitr:::sans_ext(infile), ".", format)
  if(ps2eps){
    command <- sprintf("ps2eps -f %s", infile)
    system(command)
    infile <- paste0(knitr:::sans_ext(infile), ".eps")
  }
  command <- sprintf("convert -trim -transparent white -density 300 %s %s %s", moreopts, infile, outfile)
#   command <- ifelse(format=="gif", sprintf("convert -trim %s %s", psfile, pngfile),
#                     sprintf("gs -r300 -dEPSCrop -dTextAlphaBits=4 -sDEVICE=png16m -sOutputFile=%s -dBATCH -dNOPAUSE %s", pngfile, psfile)
#                     #sprintf("gs -r300 -dTextAlphaBits=4 -sDEVICE=png16m -sOutputFile=%s -dBATCH -dNOPAUSE %s", pngfile, psfile)
#                     )
  # ça coupe le bas, finalement: convert -density 300 in.eps out.png
  # aussi -resize 600x400 -transparent white

  system(command)
  return(invisible())
}