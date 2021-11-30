#########################################################################################################################
#     Extract and list of all packages required to install a R library recursively     
#                                                                                      
# The function extracts the information on 'Depends' and 'imports' from package        
# description, and for each library mentioned in 'Depends' and 'imports', it will      
# extract its 'Depends' and 'imports' recursively.                                     
#                                                                                      
#                                                                                      
# As a reference, the below is an example of package description of 'Hmisc'            
# Package: Hmisc
# Version: 4.6-0
# Date: 2021-10-05
# Title: Harrell Miscellaneous
# Authors@R: c(person(given = "Frank E", family = "Harrell Jr", role = c("aut", "cre"), email =
#                       "fh@fharrell.com", comment = c(ORCID = "0000-0002-8271-5493")), 
#              person(given = "Charles", family = "Dupont", role = "ctb", email = "charles.dupont@vumc.org",
#                       comment = "contributed several functions and maintains latex functions"))
# Maintainer: Frank E Harrell Jr <fh@fharrell.com>
# Depends: lattice, survival (>= 3.1-6), Formula, ggplot2 (>= 2.2)
# Imports: methods, latticeExtra, cluster, rpart, nnet, foreign, gtable, grid, gridExtra,
#           data.table, htmlTable (>= 1.11.0), viridis, htmltools, base64enc
# Suggests: acepack, chron, rms, mice, tables, knitr, plotly (>= 4.5.6), rlang, plyr, VGAM
# Description: Contains many functions useful for data analysis, high-level graphics, utility
# operations, functions for computing sample size and power, simulation, importing and
# annotating datasets, imputing missing values, advanced table making, variable
# clustering, character string manipulation, conversion of R objects to LaTeX and html
# code, and recoding variables.
# License: GPL (>= 2)
# LazyLoad: Yes
# URL: https://hbiostat.org/R/Hmisc/
#   Encoding: UTF-8
# RoxygenNote: 7.1.1
# NeedsCompilation: yes
# Packaged: 2021-10-06 18:12:56 UTC; harrelfe
# Author: Frank E Harrell Jr [aut, cre] (<https://orcid.org/0000-0002-8271-5493>), Charles Dupont
# [ctb] (contributed several functions and maintains latex functions)
# Repository: CRAN
# Date/Publication: 2021-10-07 09:00:02 UTC
# Built: R 3.6.2; x86_64-apple-darwin15.6.0; 2021-10-08 09:26:09 UTC; unix
############################################################################################################################


# Function: extract libraries in "Depends" and "Imports" of package description
# Not recursive, just one layer
extract_reqlib_onelayer <- function(libraryname){
  # Package description of the library
  desc <- packageDescription(libraryname)
  
  # All dependencies
  if ('Depends' %in% names(desc)){
    depends <- strsplit(desc$Depends, ", |\n|, \n")[[1]]  
  } else {
    depends <- c()
  } 
  
  # All libraries required to import
  if ('Imports' %in% names(desc)){
    imports <- strsplit(desc$Imports, ", |\n|, \n")[[1]]  
  } else {
    imports <- c()
  }
  
  comb_dep_imp <- c(depends, imports)
  reqlib <- c()
  
  # Save them in a list
  for (i in comb_dep_imp){
    if (grepl(" ", i, fixed = TRUE)){ # Take care of e.g. 'survival (>= 3.1-6)'
      tmp = strsplit(i, " ")[[1]][1]
      reqlib <- append(reqlib, tmp)
    } else {
      reqlib <- append(reqlib, i)
    }
  }
  
  return(reqlib)
}


# Function: extract all libraries in "Depends" and "Imports" related to a library
# recursively 
extract_reqlib_recursive <- function(libraryname){
  reqlib <- extract_reqlib_onelayer(libraryname)
  
  viewed_pkgs <- reqlib
  while(length(viewed_pkgs) > 0){
    tmp <- extract_reqlib_onelayer(viewed_pkgs[1])
    
    reqlib <- c(reqlib, setdiff(tmp, reqlib))
    viewed_pkgs <- c(viewed_pkgs, setdiff(tmp, reqlib))
    viewed_pkgs <- viewed_pkgs[-1]
  }
  
  return(reqlib)
}

