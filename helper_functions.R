# Helpful functions created and/or curated by jscriven

not_all_na <- function(x) any(!is.na(x)) # returns logical for at least 1 non-NA element
not_any_na <- function(x) all(!is.na(x)) # returns logical for at least 1 NA element
getunique <- function(x) unique(x[!is.na(x)]) # unique non-nans
getnonan <- function(x) x[!is.na(x)] # return non-nan vector
getuniquelen <- function(x) length(unique(x[!is.na(x)])) # length of vector of unique non-nans 
len <- function(x) length(x) # pythonic length
lookup_first <- function(df) Reduce(`|`, lapply(df[2:ncol(df)], `==`, df[,1])) # lookup elements of first column in remaining columns of dataframe

# read in excel sheets as list of dfs
# library(readxl)    
read_excel_allsheets <- function(filename, sheet_keep = ".", tibble = TRUE, cleancols = FALSE, nskip = 0) {
  sheets <- readxl::excel_sheets(filename)
  sheets <- grep(paste(sheet_keep,collapse = "|"),sheets, value = TRUE)
  if( cleancols) x <- lapply(sheets, function(X) transform(clean_names(readxl::read_excel(filename, sheet = X, skip = nskip)), sheetname = paste0(filename,X)))
  if(!cleancols) x <- lapply(sheets, function(X) transform(readxl::read_excel(filename, sheet = X, skip = nskip), sheetname = paste0(filename,X)))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}

# read in sheets of excel workbooks as list of lists of dfs
read_excel_allworkbooks <- function(path, pattern, sheet_keep = ".", cleancols = FALSE, nskip = 0) {
  filelist  <- list.files(path = path, pattern = pattern, full.names = T)
  files <- lapply(filelist , read_excel_allsheets, sheet_keep = sheet_keep, cleancols = cleancols, nskip = nskip)
  files
}

# split unidimensional object into sub-objects of equal size k
equi_split <- function(data, k=2){
  n <- length(data)
  out <- split(data, rep(1:ceiling(n/k), each=k)[1:n])
  return(out)
}

# collapse mutually exclusive dummies into categorical
dedummy = function(x)
{
    ans = integer(nrow(x))
    for(i in seq_along(x)) ans[as.logical(x[[i]])] = i
    names(x)[ans]
} 

# coalesce dplyr piped columns
coalesce_df <-function(data, ...) {
  data %>%
    select(...) %>%
    transmute(result = invoke(coalesce, .)) %>%
    bind_cols(data, .)
}

# clean html from strings
cleanhtml <- function(htmlString) {
  return(gsub("<.*?>", "", htmlString))
}
                       
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
           }})
           print("packages loaded")
}

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



replacewith <-function(df,pattern,replacement){    
    df <- as.data.frame(apply(df, 2, function(x) gsub(pattern, replacement, x)))
      df
    }

#========================================================================
# codebook functions 
#========================================================================

# create empty codebook

scrivbook <- function(
  pattern=""
  , type=""
  , data=as.data.frame(list())
  , proj=gsub('.*([0-9].)$','\\1',here())
  , sname=""
  , info_cols=""
  , name=""
  , file=""
  , mode="list"
  , dir= "../data/codebooks/"
  , replace = False){
    
    # if (name==""){
    #   name <- paste0("cb_JS",proj,"_",sname,".","xlsx")
    # } else { name <- name}
    
    if (file==""){
        file <- paste0(dir,name,".","xlsx")
    } else { file <- file}
    # print(name)

    if (mode=="list"){
      if(type=="path"){
        return(list.files(dir, pattern=paste0("^cb.*", pattern),full.names = TRUE))
      } else if (type=="name"){
        return(list.files(dir, pattern=paste0("^cb.*", pattern),full.names = FALSE))
      } else if(type=="book"){
        return(read.xlsx(list.files(dir, pattern=paste0("^cb.*", pattern),full.names = TRUE), sheet="main"))
      } else {
        return(cbind(list.files(dir, pattern=paste0("^cb.*", pattern),full.names = FALSE)
        ,list.files(dir, pattern=paste0("^cb.*", pattern),full.names = TRUE)))
      }
    } 
    
    make_codebook <- function(data){
        
        data_out <- data.frame(qid = 1:len(data)) %>%
        mutate(
            name_old = names(data)
            , description = NA
            , name_new = NA
            , display_names = NA
            , display_names_short = NA
            , comments = NA
        )
        
        if (len(info_cols)!=0){
        for (f in (info_cols)){
            data_out[[f]] <- NA
        }
        }
        
        return(data_out)
    }
    
    if (mode=="make:r"){
        return(make_codebook(data))
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
      print(name)
     return( browseURL(file))
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
