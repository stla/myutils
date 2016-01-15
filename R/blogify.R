#' HTML fragment formatter
#' 
#' Modification of \code{rmarkdown::html_fragment} in order to control \code{mathjax}
#' 
#' ceci a été corrigé quand j'ai posté l'issue, mais je ne me souviens pas de ce que c'est 
#' 
#' @importFrom rmarkdown html_document
#' @export
html_fragment <- function (number_sections = FALSE, fig_width = 7, fig_height = 5, 
          fig_retina = if (!fig_caption) 2, fig_caption = FALSE, dev = "png", 
          smart = TRUE, keep_md = FALSE, md_extensions = NULL, pandoc_args = NULL, 
          ...) 
{
  rmarkdown::html_document(number_sections = number_sections, fig_width = fig_width, 
                fig_height = fig_height, fig_retina = fig_retina, fig_caption = fig_caption, 
                dev = dev, smart = smart, keep_md = keep_md, md_extensions = md_extensions, 
                pandoc_args = pandoc_args, highlight = NULL, 
                theme = NULL, ..., template = rmarkdown:::rmarkdown_system_file("rmd/fragment/default.html"))
}

#' Convert a Rmd file to a html ready for stlapblog
#' 
#' le "local" ne résoud pas la problème avec gmp
#' par ailleurs je ne sais pas à quoi il sert...
#' 
#' @importFrom rmarkdown render
#' @importFrom stringr str_detect str_split_fixed
#' @import data.table
#' @export
blogify <- function(Rmd, date=NULL, outdir=NULL, local=TRUE, template=system.file("blogify/z_template.html", package="myutils")){
  meta <- readLines(Rmd, n=5)
  if(is.null(outdir)) outdir <- dirname(Rmd)
  if(is.null(date)){
    date <- meta[stringr::str_detect(meta, "date:")]
    date <- stringr::str_split_fixed(date, "date: ", 2)[1,2]
    date <- eval(parse(text=date))
  }
  title <- meta[stringr::str_detect(meta, "title:")]
  title <- stringr::str_split_fixed(title, "title: ", 2)[1,2]
                   title <- eval(parse(text=title))
  lines <- readLines(template)
  whichlines <- which(stringr::str_detect(lines, "%s"))
  lines[whichlines[1]] <- sprintf(lines[whichlines[1]], title)
  lines[whichlines[2]] <- sprintf(lines[whichlines[2]], title)
  lines[whichlines[3]] <- sprintf(lines[whichlines[3]], date)
  lines[whichlines[4]] <- sprintf(lines[whichlines[4]], Rmd)
  if(local) {
    html <- local({ rmarkdown::render(Rmd, output_format=html_fragment(mathjax="default", self_contained=FALSE), output_dir=outdir) })
  } else{
    html <- rmarkdown::render(Rmd, output_format=html_fragment(mathjax="default", self_contained=FALSE), output_dir=outdir)
  }
  lines[whichlines[5]] <- paste(readLines(html), collapse="\n")
  writeLines(lines, html)
  return("")  
}