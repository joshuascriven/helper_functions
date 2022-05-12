# Helpful functions created/curated by jscriven

# functions for identifying if any or all elements an object comprise NAs
not_all_na <- function(x) any(!is.na(x))
not_any_na <- function(x) all(!is.na(x))
getunique <- function(x) unique(x[!is.na(x)]) # unique non-nans
getuniquelen <- function(x) length(unique(x[!is.na(x)])) # length of vector of unique non-nans 
len <- function(x) length(x) # pythonic length
lookup_first <- function(df) Reduce(`|`, lapply(df[2:ncol(df)], `==`, df[,1])) # lookup elements of first column in remaining columns of dataframe

                       
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

# for custom margin line locations 
line2user <- function(line, side) {
  lh <- par('cin')[2] * par('cex') * par('lheight')
  x_off <- diff(grconvertX(c(0, lh), 'inches', 'npc'))
  y_off <- diff(grconvertY(c(0, lh), 'inches', 'npc'))
  switch(side,
         `1` = grconvertY(-line * y_off, 'npc', 'user'),
         `2` = grconvertX(-line * x_off, 'npc', 'user'),
         `3` = grconvertY(1 + line * y_off, 'npc', 'user'),
         `4` = grconvertX(1 + line * x_off, 'npc', 'user'),
         stop("Side must be 1, 2, 3, or 4", call.=FALSE))
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

# reorder and rename coefficients in Latex output of texreg (from https://www.r-bloggers.com/2013/10/call-them-what-you-will/)
build.ror <- function(final.rnames, name.map){
    keep <- final.rnames %in% names(name.map)
    mapper <- function(x){
    mp <- name.map[[x]] 
    ifelse(is.null(mp), x, mp)
    }
    newnames <- sapply(final.rnames, mapper)
    omit <- paste0(final.rnames[!keep], collapse="|")
    reorder <- na.omit(match(unlist(name.map), newnames[keep]))
    list(ccn=newnames, oc=omit, rc=reorder)
}

# get all variable names from list of models (from https://www.r-bloggers.com/2013/10/call-them-what-you-will/)
all.varnames.dammit <- function(model.list){
    mods <- texreg:::get.data(model.list)
    gofers <- texreg:::get.gof(mods)
    mm <- texreg:::aggregate.matrix(mods, gofers, digits=3)
    rownames(mm)
}

# color scale creator with dcf dashboards color scheme
dcf_scale <- function(n){
  colorRampPalette(c("#FF0018","#AD1510","#FB8C00","#FDD734","#5A8D55"))(n)
}

# convert character to numeric, taking level order into account
make_numeric <- function(x,lev) as.numeric(ordered(x , levels = lev ))

# return odbc database from regex search
getdb <- function(connection, string_name, locate = "exact"){
  if (locate != "exact"){
    db <- grep("qualtr",dbListTables(connection), value = TRUE)
    dbGetQuery(connection, paste0(sprintf("SELECT * FROM [%s]", db[length(db)])))
  } else {
    db <- grep(string_name,dbListTables(connection), value = TRUE)
    if (length(db) > 1){
      cat("Exact string not found! Use locate='latest' or one of the following:\n",paste0(db,collapse = "\n"))
    } else {
      dbGetQuery(connection, paste0(sprintf("SELECT * FROM [%s]", db)))
    }
  }
}

# return odbc database names from regex search
getdb_names <-  function(connection, string_name){
  grep(string_name,dbListTables(connection), value = TRUE)
}

# simple lookup function for stored id info
getKey <- function(info){
  api_key %>% filter(param == info) %>% pull(val)
}

#========================================================================
# codebook functions 
#========================================================================

# create empty codebook

scrivbook <- function(
  data=as.data.frame(list())
  , proj=gsub('.*([0-9].)$','\\1',here())
  , sname=""
  , pattern=""
  , mode="list"
  , dir= "../data/codebooks/"
  , replace = False){

  name <- paste0("cb_JS",proj,"_",sname,".","xlsx")
  file <- paste0(dir,name,".","xlsx")
  # print(name)

  if (mode=="list"){
    return(list.files(dir, pattern=paste0("cb", pattern)))
  } 
  
  make_codebook <- function(data){
    (as.data.frame(1:ncol(data)) %>%
      mutate(
        name_old = names(data)
        , description = NA
        , name_new = NA
        , iv_dv_cov = NA
        , direction = NA
        , display_names = NA
        , display_names_short = NA
        , comments = NA
    )) %>% rename(qid = `1:ncol(data)`)
    }
    
  if (mode=="make:r"){
    make_codebook(data)
  }

  # Make excel workbook
  if (mode=="make:excel"){
      
    ifelse(!dir.exists(file.path(dir)), dir.create(file.path(dir), recursive = TRUE), "dir already exists!")

    codebook <- make_codebook(data,name)
  
    if (!file.exists(file)) {
      wb <- createWorkbook()
      
      describe_data <- sprintf(
        "This file contains questions and labels for the %s project.", name)
      
      addWorksheet(wb, "data_description")
      writeData(wb, "data_description",describe_data)
      
      addWorksheet(wb, "main")
      writeData(wb, "main",codebook)
      
      saveWorkbook(wb,file, overwrite = replace)
    }

    browseURL(file)
  }

  # open existing codebook in excel
  if (mode=="view"){
    browseURL(file)
  }
}


#load this rmd YAML and Rmd template to the clipboard
# out <- '---
# title: "Title"
# author: "Joshua Scriven"
# date: Sys.Date()
# # output: ioslides_presentation
# # output: word_document
# # output: pdf_document 
# 
# #   # requires install.packages("revealjs")
#   # output:
# #   revealjs::revealjs_presentation: 
# #     theme: solarized
# #     highlight: tango
# #     transition: slide
# 
# #Requires install.packages("rmdformats ")
# # output: rmdformats::readthedown
# # output: rmdformats::html_clean
# 
# # output:
# #   slidy_presentation:
# #     # theme: darkly
# #     # theme: lumen # good
# #     # theme: readable
# #     theme: cerulean # good
#     # theme: journal
#     # theme: flatly
#     # theme: spacelab
#     # theme: united
#     # theme: cosmo #good
#     # theme: paper
#     # theme: sandstone
#     # theme: simplex
#     # theme: yeti # good
# # output: slidy_presentation
# output: html_notebook
# # output: powerpoint_presentation
# ---
# 
# ```{r setup, include=FALSE}
# knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message=FALSE)
# ```
# 
# ```{r custom_functions, include=FALSE}
# source("https://raw.githubusercontent.com/joshuascriven/helper_functions/main/helper_functions.R")
# ```
# 
# ```{r, include=FALSE}
# packageloader(c("openxlsx", "tidyverse", "dplyr", "knitr", "stargazer", "gtsummary", "english", "scales", "ggpubr", "broom", "AICcmodavg", "lmtest", "sandwich", "reshape2"))
# ```'
# # Windows or Mac
# clipr::write_clip(out, allow_non_interactive = TRUE)
