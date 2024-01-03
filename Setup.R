## CLEAR ALL
# Please restart R Session before proceeding

source("packages.R") ## load packages

source("Functions.R") ## load functions

source("Get_Results.R") ## get results
names(resultlist) <- c("sim1_21", "sim2_21", "sim3_21", "sim4_21", "sim5_21", "sim6_21", "sim7_21", "sim8_21",
  "sim1_50", "sim2_50", "sim3_50", "sim4_50", "sim5_50", "sim6_50", "sim7_50", "sim8_50",
  "sim1_100", "sim2_100", "sim3_100", "sim4_100", "sim5_100", 
                       "sim6_100", "sim7_100", "sim8_100", )

rmarkdown::render("Results-html.Rmd") # make results notebook


## Make Figures and Tables
numbers_of_periods = c(55, 100) # define analysis types

for (i in seq_along(resultlist)) {
  current_object <- resultlist[[i]]
  current_name <- names(resultlist)[i]
  
  # Generate and save combined deviation / density plot
  plot_combined(current_object, T)
  # Get DGP number for DGP plotting
  dgp_number <- as.numeric(substring(as.character(current_name), 4, 4))
  nperiods <- current_object$nperiods[1]
  
  table_name <- paste0("DGP_", dgp_number, "_", nperiods, "_table")
  table <- analyze_sim_results(current_object)
  
  # assign to object for exporting
  assign(table_name, table)
  print.data.frame(table)
}

# List all objects that contain the word "table"
all_table_names <- ls(pattern = "_table")

# Filter out the functions, keeping only data objects
non_function_table_names <- Filter(function(x) {
  !is.function(get(x))
}, all_table_names)

# Retrieve the non-function objects and create a list
table_objects <- mget(non_function_table_names)

# On my machine only, export figures and tables into Overleaf
if (Sys.info()[7] == "ts") {
  source("Export.R")
}
