## CLEAR ALL
rm(list = ls(all = TRUE)) 
## load packages
source("packages.R")
## load custom functions
source("Functions.R")


# Run Main Scripts
source("Get_Results.R")

# Make Figures
source("Figures.R")

# Make Tables
source("Tables.R")

# On my machine only, export figures and tables into Overleaf
if (Sys.info()[7] == "ts") {
  grateful::scan_packages()
  # rscript("Export.R", show = F)
  source("Export.R")
  setwd("~/Git/matrix-completion")
}
