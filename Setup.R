## CLEAR ALL
rm(list = ls(all = TRUE)) 
## load packages
source("packages.R")
## load custom functions
source("Functions.R")


# Run Main Scripts
rscript("Get_Results.R")

# Make Figures
rscript("Figures.R", show = T)

# Make Tables
rscript("Tables.R", show = F)

# On my machine only, export figures and tables into Overleaf
if (Sys.info()[7] == "ts") {
  grateful::scan_packages()
  # rscript("Export.R", show = F)
  source("Export.R")
  setwd("~/Git/matrix-completion")
}
