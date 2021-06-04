# Helpful functions created/curated by jscriven

# formula paster
formfunc <- function(dv, ivs){
  as.formula(
    paste(dv, 
          paste(ivs, collapse = " + "), 
          sep = " ~ "))
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

#load this rmd YAML header with commented out options to the clipboard
writeClipboard('---
title: "LOC Data Request 2"
author: "Joshua Scriven"
date: "6/3/2021"
# output: ioslides_presentation
# output: word_document

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
---')