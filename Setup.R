## CLEAR ALL
rm(list = ls(all = TRUE)) 

source("packages.R") ## load packages

source("Functions.R") ## load functions

source("Get_Results.R") ## get results

# Make Figures
plot_combined(1, dynamic = F, T) # dgp 1
plot_combined(2, dynamic = T, T) # dgp 2
plot_combined(3, dynamic = F, T) # dgp 3
plot_combined(4, dynamic = F, T) # dgp 4
plot_combined(5, dynamic = T, T) # dgp 5
plot_combined(6, dynamic = T, T) # dgp 6
plot_combined(7, dynamic = T, T) # dgp 7

## Make Tables
analysis_types = c("static", "dynamic") # define analysis types

for (i in 1:7) { # loop over DGPs and types, assign and print
  dgp <- get(paste0("sim", i))
  
  # Loop over analysis types
  for (analysis_type in analysis_types) {
    table_name <- paste0("sim", i, "_table", ifelse(analysis_type == "static", "1", "2"))
    table <- analyze_sim_results(dgp, analysis_type)
    
    # assign to ibject for exporting
    assign(table_name, table)
    print.data.frame(table)
  }
}

# On my machine only, export figures and tables into Overleaf
if (Sys.info()[7] == "ts") {
  grateful::scan_packages()
  # rscript("Export.R", show = F)
  source("Export.R")
  setwd("~/Git/matrix-completion")
}
