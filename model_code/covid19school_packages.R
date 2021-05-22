# Packages necessary
check.and.install.pkgs <- function(pkgs){
  new.packages <- pkgs[!pkgs %in% installed.packages()[,"Package"]]
  if(length(new.packages)) install.packages(new.packages, dependencies = TRUE)
  suppressPackageStartupMessages(invisible(lapply(pkgs, library, character.only = TRUE)))
}
check.and.install.pkgs(c("ggplot2",
                         "data.table", 
                         "reshape2", 
                         "scatterplot3d", 
                         "dplyr",
                         "ggthemes", # for theme_publication
                         "truncnorm",
                         "microbenchmark", 
                         "parallel")) 
