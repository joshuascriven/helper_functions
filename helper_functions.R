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
packageloader <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)