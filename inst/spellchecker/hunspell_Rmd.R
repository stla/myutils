hunspell_Rmd <- function(file){
  require(stringr)
  require(hunspell)
  lines <- str_trim(readLines(file)) 
  # replace chunks 
  begins <- which(str_sub(lines, 1, 5)=="```{r")
  ends <- which(str_sub(lines, 1, 3)=="```")
  ends <- ends[which(! ends %in% begins)]
  begins <- begins-1L
  begins <- c(begins, length(lines))
  h <- hunspell(lines[1:begins[1]])
  for(j in 1:(length(begins)-1L)){
    h <- c(h, hunspell(lines[ends[j]:begins[j+1]]))
  }
  return(unlist(h))
  }

setwd("~/Github/myutils/inst/spellchecker")

hunspell_Rmd("Rmd.Rmd")