### This script copies all figures and tables into Overleaf

### Tables
setwd("/Users/ts/Library/CloudStorage/Dropbox/Apps/Overleaf/Thesis/Tables")

## DGP 1
save_table_results(sim1_table1, 
                   caption = "Simulation 1, Point Estimates of  $\\tau$", 
                   file_name = "Sim1_stat.tex")
save_table_results(sim1_table2, 
                   caption = "Simulation 1, Point Estimates of  $\\tau^{ES}$", 
                   file_name = "Sim1_dyn.tex")

## DGP 2
save_table_results(sim2_table1, 
                   caption = "Simulation 2, Point Estimates of  $\\tau$", 
                   file_name = "Sim2_stat.tex")
save_table_results(sim2_table2, 
                   caption = "Simulation 2, Point Estimates of  $\\tau^{ES}$", 
                   file_name = "Sim2_dyn.tex")

## DGP 1
save_table_results(sim3_table1, 
                   caption = "Simulation 3, Point Estimates of  $\\tau$", 
                   file_name = "Sim3_stat.tex")
save_table_results(sim3_table2, 
                   caption = "Simulation 3, Point Estimates of  $\\tau^{ES}$", 
                   file_name = "Sim3_dyn.tex")

## DGP 4
save_table_results(sim4_table1, 
                   caption = "Simulation 4, Point Estimates of  $\\tau$", 
                   file_name = "Sim4_stat.tex")
save_table_results(sim4_table2, 
                   caption = "Simulation 4, Point Estimates of  $\\tau^{ES}$", 
                   file_name = "Sim4_dyn.tex")

## DGP 5
save_table_results(sim5_table1, 
                   caption = "Simulation 5, Point Estimates of  $\\tau$", 
                   file_name = "Sim5_stat.tex")
save_table_results(sim5_table2, 
                   caption = "Simulation 5, Point Estimates of  $\\tau^{ES}$", 
                   file_name = "Sim5_dyn.tex")

## DGP 6
save_table_results(sim6_table1, 
                   caption = "Simulation 6, Point Estimates of  $\\tau$", 
                   file_name = "Sim6_stat.tex")
save_table_results(sim6_table2, 
                   caption = "Simulation 6, Point Estimates of  $\\tau^{ES}$", 
                   file_name = "Sim6_dyn.tex")

## DGP 7
save_table_results(sim7_table1, 
                   caption = "Simulation 7, Point Estimates of  $\\tau$", 
                   file_name = "Sim7_stat.tex")
save_table_results(sim7_table2, 
                   caption = "Simulation 7, Point Estimates of  $\\tau^{ES}$", 
                   file_name = "Sim7_dyn.tex")


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
