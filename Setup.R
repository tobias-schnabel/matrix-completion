## CLEAR ALL
# Please restart R Session before proceeding

source("packages.R") ## load packages

source("Functions.R") ## load functions

source("Get_Results.R") ## get results
names(resultlist) <- c("sim1_100", "sim2_100", "sim3_100", "sim4_100", "sim5_100", 
                       "sim6_100", "sim7_100", "sim8_100", "sim1_55", "sim2_55", 
                       "sim3_55", "sim4_55", "sim5_55", "sim6_55", "sim7_55", "sim8_55")

rmarkdown::render("Results.Rmd") # make results notebook

# Make Figures
plot_combined(1, T) # dgp 1
plot_combined(2, T) # dgp 2
plot_combined(3, T) # dgp 3
plot_combined(4, T) # dgp 4
plot_combined(5, T) # dgp 5
plot_combined(6, T) # dgp 6
plot_combined(7, T) # dgp 7
plot_combined(8, T) # dgp 8

## Make Tables
numbers_of_periods = c(55, 100) # define analysis types

for (result in resultlist) { # loop over DGPs and types, assign and print
  
  # Get DGP number for DGP plotting
  dgp_number <- substring(as.character(), 4, 4)
  dgp <- get(paste0("sim", i))
  
  table_name <- paste0("sim", i, "_table", ifelse(analysis_type == "static", "1", "2"))
  table <- analyze_sim_results(dgp, analysis_type)
  
  # assign to object for exporting
  assign(table_name, table)
  print.data.frame(table)
}

# On my machine only, export figures and tables into Overleaf
if (Sys.info()[7] == "ts") {
  source("Export.R")
}
