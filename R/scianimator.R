#' Generate LaTeX figures in png format
#' 
#' @importFrom tikzDevice tikz
#' @importFrom jsonlite toJSON
#' @export
tikz2png <- function(code, nplots, prefix="Rplot", outdir=getwd(), folder="assets/images/", format="png", ...){
  texfiles <- myutils::charseq(nplots, prefix=prefix, suffix=".tex")
  owd <- setwd(outdir); on.exit(setwd(owd))
  if(!all(file.exists(texfiles))){
    tikz(attr(texfiles, "string"), standAlone=TRUE, onefile=FALSE, ...)
      code(nplots)
    dev.off()
  }
  # pdf compilation
  pdffiles <- stringr::str_replace(texfiles, ".tex", ".pdf")
  if(!all(file.exists(pdffiles))){
    sapply(texfiles, function(tex) tools::texi2dvi(tex, pdf=TRUE, clean=TRUE))
  }
  # conversion png ImageMagick 
  pngfiles <- stringr::str_replace(texfiles, ".tex", ".png")
  for(i in 1:nplots){
    command <- sprintf("convert -density 300 -resize %s %s %s", "30%", pdffiles[i], pngfiles[i])
    system(command)
  }
  return( toJSON(paste0(folder,pngfiles)) )
}

#' Create html and js files for Scianimator
#' 
#' @export
scianim <- function(name, imgs, outdir=getwd()){
  html <- readLines(system.file("scianimator/index.html", package="myutils"))
  js <- readLines(system.file("scianimator/assets/js/index.js", package="myutils"))
  line <- which(stringr::str_detect(html, "%s"))
  html[line] <- sprintf(html[line], name)
  line <- which(stringr::str_detect(js, "%s"))
  js[line] <- sprintf(js[line], imgs)
  owd <- setwd(outdir); on.exit(setwd(owd))
  writeLines(html, sprintf("%s.html", name))
  writeLines(js, sprintf("%s.js", name)) 
  return(invisible())
}
