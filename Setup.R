##CLEAR ALL
rm(list = ls(all = TRUE)) 

## install required packages
# Package names
packages <- c("tidyverse", "xtable", "stargazer", "gsynth", "purrr", "callr")

# load packages
invisible(lapply(packages, library, character.only = TRUE))

# Run main scripts


# Make Figures


# Make Tables

# On my machine only, export figures and tables into Overleaf
if (Sys.info()[7] == "ts") {
  rscript("Export.R", show = F)
  setwd("~/Git/matrix-completion")
}