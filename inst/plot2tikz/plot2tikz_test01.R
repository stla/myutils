setwd("~/Github/myutils/inst/plot2tikz")

# faire dev.off() quand ça plante 

plotCode <- function(){
  curve(x^2, from=-1, to=1, asp=1, axes=FALSE, ylab=NA) # attention car il met x^2 dans ylab 
  xlabs_at <- seq(-1, 1, by=0.5)
  axis(1, at=xlabs_at, labels=dollarify()(xlabs_at))
  ylabs_at <- seq(0, 1, by=0.2)
  axis(2, at=ylabs_at, labels=dollarify()(ylabs_at))
  text(0.65, 0.8, dollarify()("f(x)=x\\\\^2"))
  return(invisible())
}

# pdf -packages par défaut
plot2tikz(plotCode, compile=FALSE,  overwrite = TRUE, 
                  documentDeclaration ="\\documentclass[12pt]{standalone}\n", 
                  width=7, height=5)

# pdf - packages à la main (tikz pas nécessaire - automatique si absent)
plot2tikz(plotCode, compile=FALSE,  overwrite = TRUE, 
          packages=c("\\usepackage{tikz}\n", "\\usepackage[active,tightpage,psfixbb]{preview}\n", 
                     "\\PreviewEnvironment{pgfpicture}\n", "\\setlength\\PreviewBorder{0pt}\n", 
                     "\\usepackage{amssymb}\n"),
          documentDeclaration ="\\documentclass[12pt]{standalone}\n", 
          width=7, height=5)

# eps -packages par défaut avec eps
plot2tikz(plotCode, format="eps", compile=FALSE, clean=TRUE, overwrite=TRUE, 
          documentDeclaration ="\\documentclass[12pt]{standalone}\n", 
          width=7, height=5)

# eps - packages à la main avec eps
plot2tikz(plotCode, format="eps", compile=FALSE, clean=TRUE, overwrite=TRUE, 
          documentDeclaration ="\\documentclass[12pt]{standalone}\n", 
          packages="\\usepackage{amssymb}\n", 
          width=7, height=5)

# compile eps 
plot2tikz(plotCode, format="eps", compile=TRUE, clean=TRUE, overwrite=TRUE, 
          documentDeclaration ="\\documentclass[12pt]{standalone}\n", 
          width=7, height=5)
