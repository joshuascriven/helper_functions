# Helpful functions created/curated by jscriven

# merge polygons in sf by grouping id
st_union_by <- function(geo, group) {
   # browser()
   geo
   group
 
   y2 = list()
   #loop over by groups and merge units
   for (i in unique(group)) {
     #which units
     z = geo[group == i]
 
     #merge
     y = Reduce(st_union, z)
     y2[[i]] = y
   }
 
   st_sfc(y2)
 }

# inclusive version of seq()
seqlast <- function (from, to, by) 
{
  vec <- do.call(what = seq, args = list(from, to, by))
  if ( tail(vec, 1) != to ) {
    return(c(vec, to))
  } else {
    return(vec)
  }
}

# formula paster
formfunc <- function(dv, ivs,int){
  if(missing(int)) {
    as.formula(
      paste(dv, 
            paste(ivs, collapse = " + "), 
            sep = " ~ "))    
  } else {
    as.formula(
      paste(dv, 
            paste(c(ivs,int), collapse = " + "), 
            sep = " ~ "))
  }

}

# mode function
getmode <- function(v) {
   uniqv <- unique(v)
   uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Pythonic zip function
zip <- function(...){
  Map(list,...)}

# print two objects as a single-line pair
print2 <- function(...){
  do.call(cat,c(list(...),"\n"))}

# (install and) load installed packages
packageloader <- function(...){
  lapply(...,
         FUN = function(x) {
           if (!require(x, character.only = TRUE)) {
             install.packages(x, dependencies = TRUE)
             library(x, character.only = TRUE)
           }})}

# place text within file name after directory externally set
filemaker <- function(filename,format){
  paste0(data_dir,filename,".",format)
}

# color scale creator with dcf dashboards color scheme
dcf_scale <- function(n){
  colorRampPalette(c("#FF0018","#AD1510","#FB8C00","#FDD734","#5A8D55"))(n)
}

#load this rmd YAML and Rmd template to the clipboard
out <- '---
title: "Title"
author: "Joshua Scriven"
date: Sys.Date()
# output: ioslides_presentation
# output: word_document
# output: pdf_document 

#   # requires install.packages("revealjs")
  # output:
#   revealjs::revealjs_presentation: 
#     theme: solarized
#     highlight: tango
#     transition: slide

#Requires install.packages("rmdformats ")
# output: rmdformats::readthedown
# output: rmdformats::html_clean

# output:
#   slidy_presentation:
#     # theme: darkly
#     # theme: lumen # good
#     # theme: readable
#     theme: cerulean # good
    # theme: journal
    # theme: flatly
    # theme: spacelab
    # theme: united
    # theme: cosmo #good
    # theme: paper
    # theme: sandstone
    # theme: simplex
    # theme: yeti # good
# output: slidy_presentation
output: html_notebook
# output: powerpoint_presentation
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message=FALSE)
```

```{r custom_functions, include=FALSE}
source("https://raw.githubusercontent.com/joshuascriven/helper_functions/main/helper_functions.R")
```

```{r, include=FALSE}
packageloader(c("openxlsx", "tidyverse", "dplyr", "knitr", "stargazer", "gtsummary", "english", "scales", "ggpubr", "broom", "AICcmodavg", "lmtest", "sandwich", "reshape2"))
```'
# Windows or Mac
clipr::write_clip(out, allow_non_interactive = TRUE)
