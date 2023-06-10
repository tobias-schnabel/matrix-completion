## CLEAR ALL
rm(list = ls(all = TRUE)) 
## load packages
source("packages.R")

### Set RNG parameters
globalseed  = 12345 # All simulations will use a global seed of 12345
set.seed(globalseed)

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
  grateful::scan_packages()
  # rscript("Export.R", show = F)
  source("Export.R")
  setwd("~/Git/matrix-completion")
}
