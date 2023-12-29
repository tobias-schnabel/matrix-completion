### This script copies all figures and tables into Overleaf

#### Tables ####
setwd("/Users/ts/Library/CloudStorage/Dropbox/Apps/Overleaf/MC Paper/Tables")

for (name in names(table_objects)) {
  # Access the object itself
  tab <- table_objects[[name]]
  
  # Check if results have 7 rows (static), or 6 (dynamic)
  if (nrow(tab) == 7) {
    param = "$\\tau$"
  } else {
    param = "$\\tau^{rel}_{t=0$"
  }
  
  # Extract the DGP number from the object name
  dgp_num <- as.numeric(sub(".*?(\\d+).*", "\\1", name))
  # Extract the number of periods
  n_periods <- sub(".*?\\d+_([0-9]+).*", "\\1", name)
  
  # Build filename
  fname = paste0("DGP-", dgp_num, "-", n_periods, ".tex" )
  cap = paste0("DGP ", dgp_num, " , ", n_periods, 
               " Periods, Point Estimates of ", param)
  # Save table
  save_table_results(tab,
                     caption = cap, file_name = fname)
}



#### Figures ####
setwd("/Users/ts/Library/CloudStorage/Dropbox/Apps/Overleaf/MC Paper/Figures")
# simulate one of each DGP used for DGP plots
periods <- c(55, 100)
dgp_functions <- list(dgp_1_sim = dgp_1_sim, 
                      dgp_2_sim = dgp_2_sim, 
                      dgp_3_sim = dgp_3_sim, 
                      dgp_4_sim = dgp_4_sim, 
                      dgp_5_sim = dgp_5_sim, 
                      dgp_6_sim = dgp_6_sim, 
                      dgp_7_sim = dgp_7_sim, 
                      dgp_8_sim = dgp_8_sim)

# Loop over each DGP function and period to generate the simulations
for (dgp in names(dgp_functions)) {
  for (p in periods) {
    # Create the variable name
    var_name <- paste0("dgp", sub("dgp_", "", dgp), "_", p)
    
    # Generate the simulation and assign it to the variable
    assign(var_name, dgp_functions[[dgp]](nobs = 500, nperiods = p))
  }
}

# Collect simulations in list
pattern <- "^dgp[0-9]+_sim_[0-9]+$"
object_names <- ls(pattern = pattern)
object_list <- mget(object_names)

## Plot and export each
for (name in names(object_list)) {
  dgp_num = as.numeric(substring(as.character(name), 4, 4))
  plot <- dgp_plot(object_list[[name]], sim_num = dgp_num)
  
  # Construct the filename for the plot
  fp = "/Users/ts/Library/CloudStorage/Dropbox/Apps/Overleaf/MC Paper/Figures/DGP"
  filename = paste0(name, ".png")
  ggsave(filename, plot = plot, path = fp,
         width = 18, height = 10, units = "cm")
}


# Remove the objects from the global environment
rm(list = object_names)


### Export Package References
setwd("~/Git/matrix-completion")
bib_path = "/Users/ts/Library/CloudStorage/Dropbox/Apps/Overleaf/Thesis/Tex/"

# Write the package references to a BibTeX file
grateful::scan_packages() # get loaded packages to cite
grateful::scan_packages("MarcusSantAnna2020") # get loaded packages to cite
suppressMessages(grateful::cite_packages(output = "file",
              pkgs = "All",
              out.dir = bib_path,
              omit = NULL,
              cite.tidyverse = T,
              dependencies = T,
              include.RStudio = T,
              bib.file = "packages"))
suppressMessages(grateful::cite_packages(output = "citekeys",out.dir = bib_path,
                        pkgs = c("MarcusSantAnna2020", "gsynth", "MCPanel"),
                        bib.file = "packages-2"))

## Modify all entries to have type "Manual"
# Read file
bib_file = "/Users/ts/Library/CloudStorage/Dropbox/Apps/Overleaf/Thesis/Tex/packages.bib"
bib_entries = readLines(bib_file)
# Modify the entry types to "Manual"
modified_entries = gsub("@[^}]+\\{", "@Manual{", bib_entries)
writeLines(modified_entries, con = bib_file)

setwd("~/Git/matrix-completion")
file.copy("truevals.R", "/Users/ts/Library/CloudStorage/Dropbox/Apps/Overleaf/Thesis/Tex/truevals.R",
          overwrite = T) # copy code to overleaf
