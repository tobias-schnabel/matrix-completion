##CLEAR ALL
rm(list = ls(all = TRUE)) 

# Package names
packages <- c("tidyverse", "xtable", "stargazer", "purrr", "callr", "ggplot2",
              "panelView", "devtools", "latex2exp", 
              "gsynth", "did2s", "DIDmultiplegt", "fixest", "DRDID", "staggered")

# comment in and execute to install CRAN packages
# installed_packages <- packages %in% rownames(installed.packages())
# if (any(installed_packages == FALSE)) {
#   install.packages(packages[!installed_packages])
# }

# load packages
invisible(lapply(packages, library, character.only = T))

# comment in and execute to install MC packages by Susan Athey
# devtools::install_github("susanathey/MCPanel", "synth-inference/synthdid")
library(synthdid)
library(MCPanel)
library(devtools, latex2exp, panelView)

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