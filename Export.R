### This script copies all figures and tables into Overleaf

### Tables
setwd("/Users/ts/Library/CloudStorage/Dropbox/Apps/Overleaf/Thesis/Tables")

## DGP 1
save_table_results(sim1_table1, 
                   caption = "Simulation 1, Static Estimates", 
                   file_name = "Sim1_stat.tex")
save_table_results(sim1_table2, 
                   caption = "Simulation 1, Dynamic Estimates", 
                   file_name = "Sim1_dyn.tex")

## DGP 2
save_table_results(sim2_table1, 
                   caption = "Simulation 2, Static Estimates", 
                   file_name = "Sim2_stat.tex")
save_table_results(sim2_table2, 
                   caption = "Simulation 2, Dynamic Estimates", 
                   file_name = "Sim2_dyn.tex")

## DGP 1
save_table_results(sim3_table1, 
                   caption = "Simulation 3, Static Estimates", 
                   file_name = "Sim3_stat.tex")
save_table_results(sim3_table2, 
                   caption = "Simulation 3, Dynamic Estimates", 
                   file_name = "Sim3_dyn.tex")

## DGP 4
save_table_results(sim4_table1, 
                   caption = "Simulation 4, Static Estimates", 
                   file_name = "Sim4_stat.tex")
save_table_results(sim4_table2, 
                   caption = "Simulation 4, Dynamic Estimates", 
                   file_name = "Sim4_dyn.tex")

## DGP 5
save_table_results(sim5_table1, 
                   caption = "Simulation 5, Static Estimates", 
                   file_name = "Sim5_stat.tex")
save_table_results(sim5_table2, 
                   caption = "Simulation 5, Dynamic Estimates", 
                   file_name = "Sim5_dyn.tex")

## DGP 6
save_table_results(sim6_table1, 
                   caption = "Simulation 6, Static Estimates", 
                   file_name = "Sim6_stat.tex")
save_table_results(sim6_table2, 
                   caption = "Simulation 6, Dynamic Estimates", 
                   file_name = "Sim6_dyn.tex")

## DGP 7
save_table_results(sim7_table1, 
                   caption = "Simulation 7, Static Estimates", 
                   file_name = "Sim7_stat.tex")
save_table_results(sim7_table2, 
                   caption = "Simulation 7, Dynamic Estimates", 
                   file_name = "Sim7_dyn.tex")





### Export Package References
setwd("~/Git/matrix-completion")
bib_path = "/Users/ts/Library/CloudStorage/Dropbox/Apps/Overleaf/Thesis/Tex/"

# Write the package references to a BibTeX file
grateful::cite_packages(output = "file",
              out.dir = bib_path,
              omit = NULL,
              cite.tidyverse = T,
              dependencies = T,
              include.RStudio = T,
              bib.file = "packages")

## Modify all entries to have type "Manual"
# Read file
bib_file = "/Users/ts/Library/CloudStorage/Dropbox/Apps/Overleaf/Thesis/Tex/packages.bib"
bib_entries = readLines(bib_file)
# Modify the entry types to "Manual"
modified_entries = gsub("@[^}]+\\{", "@Manual{", bib_entries)
writeLines(modified_entries, con = bib_file)
