#' Convert (e)ps to png/gif
#' 
#' 
#' @export
ps2img <- function(psfile, format="gif", ps2eps=FALSE){
  pngfile <- paste0(knitr:::sans_ext(psfile), ".", format)
  if(ps2eps){
    command <- sprintf("ps2eps -f %s", psfile)
    system(command)
    psfile <- paste0(knitr:::sans_ext(psfile), ".eps")
  }
  command <- ifelse(format=="gif", sprintf("convert -trim %s %s", psfile, pngfile),
                    sprintf("gs -r300 -dEPSCrop -dTextAlphaBits=4 -sDEVICE=png16m -sOutputFile=%s -dBATCH -dNOPAUSE %s", pngfile, psfile)
                    #sprintf("gs -r300 -dTextAlphaBits=4 -sDEVICE=png16m -sOutputFile=%s -dBATCH -dNOPAUSE %s", pngfile, psfile)
                    )
  # ça coupe le bas, finalement: convert -density 300 in.eps out.png
  # aussi -resize 600x400 -transparent white

  system(command)
  return(invisible())
}