##CLEAR ALL
rm(list = ls(all = TRUE)) 

# Package names
packages <- c("tidyverse", "xtable", "stargazer", "purrr", "callr", "ggplot2",
              "gsynth", "did2s", "DIDmultiplegt", "fixest", "DRDID", "staggered")

# comment in and execute to install oackages
# install.packages(packages[!installed_packages])

# load packages
invisible(lapply(packages, library, character.only = T))

# Run Main Scripts
rscript("Functions.R")
rscript("Data_Setup.R")
rscript("Analysis.R")

# Make Figures
rscript("Figures.R", show = T)

# Make Tables
rscript("Tables.R", show = F)

# On my machine only, export figures and tables into Overleaf
if (Sys.info()[7] == "ts") {
  rscript("Export.R", show = F)
  setwd("~/Git/matrix-completion")
}