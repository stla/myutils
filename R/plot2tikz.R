#' Enclose between dollars
#' 
#' see http://stackoverflow.com/questions/20326946/how-to-put-ggplot2-ticks-labels-between-dollars
#'
#' @export 
dollarify <- function(){
  function(x) paste0("$",x,"$")
}

#' Format to LaTeX 
#' 
#' @details \usepackage[ddmmyyyy]{datetime}
#' 
#' @export
datify <- function(){
  function(x){
    split <- stringr::str_split_fixed(as.character(x),"-",3)
    out <- character(nrow(split))
    for(i in 1:length(out)){
      out[i] <- paste0("\\formatdate{", split[i,3], "}{", split[i,2], "}{", split[i,1], "}")
    }
    out
  }
}

#' Generate LaTeX figures 
#' 
#' to do: sortie eps (commenter les 3 lignes 
#' "usepackage[active",  "PreviewEnvironment", "setlengthPreviewBorder" et 
#' ajouter "pagenumbering{gobble}")
#' faire des try 
#'  compile=FALSE fait quand même un pre-compile ! 
#'  width et height pour ps ? 
#' 
#' @param code a function creating the plot
#' @param name the name of the file
#' @param outdir output directory
#' @param compile the tex file
#' @param format the output format, if \code{compile=TRUE}
#' @param packages NULL to use default packages
#' @param clean logical, if \code{TRUE}, auxiliary files are removed 
#' @param ... arguments passed to \code{\link{tikz}}
#' 
#' @importFrom tikzDevice tikz
#' @importFrom tools texi2dvi
#' @importFrom stringr str_replace
#' @export
plot2tikz <- function(code, filename="Rplot", outdir=getwd(), overwrite=FALSE, format="pdf", packages=NULL, compile=TRUE, clean=FALSE, ...){
  texfile <- paste0(filename, ".tex")
  owd <- setwd(outdir); on.exit(setwd(owd))
  if(overwrite || !file.exists(texfile)){
#     if(!"packages" %in% names(list(...))){
#       packages <- getOption("tikzLatexPackages")
#       extra.args <- list(...)
#     } else {
#       extra.args0 <- list(...)
#       extra.args <- extra.args0[!names(extra.args0) %in% "packages"]
#       packages <- extra.args0$packages
#     }
#     do.call(function(...) tikz(texfile, standAlone=TRUE, onefile=FALSE, packages=packages, ...), extra.args)
    if(is.null(packages)){
      if(format=="pdf") packages <- getOption("tikzLatexPackages")
      if(format=="eps") packages <- c("\\thispagestyle{empty}\n", "\\usepackage{tikz}\n")
    } else {
      if(!"\\usepackage{tikz}\n" %in% packages){
        packages <- c("\\usepackage{tikz}\n", packages)
      }
    }
    tikz(texfile, standAlone=TRUE, onefile=FALSE, packages=packages, ...)
    code()
    dev.off()
  }
  if(compile){
    message("Compilation...")
    if(format=="pdf"){
      # pdf compilation
      pdffile <- stringr::str_replace(texfile, ".tex", ".pdf")
      if(overwrite || !file.exists(pdffile)){
        tools::texi2dvi(texfile, pdf=TRUE, clean=clean)
      }
    } else if(format=="eps"){
      psfile <- stringr::str_replace(texfile, ".tex", ".ps")
      if(overwrite || !file.exists(psfile)){
        tools::texi2dvi(texfile, pdf=FALSE, clean=clean)
        command <- sprintf("dvips %s.dvi", filename)
        system(command)
        command <- ifelse(overwrite, sprintf("ps2eps -f %s.ps", filename),  sprintf("ps2eps %s.ps", filename))
        system(command)
      }
    }
  }
  # 
  message(sprintf("Output file(s): %s", texfile))
  return(invisible())
}
