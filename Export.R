### This script copies all figures and tables into Overleaf

### Figures
setwd("/Users/ts/Library/CloudStorage/Dropbox/Apps/Overleaf/Thesis/Figures")


### Tables
setwd("/Users/ts/Library/CloudStorage/Dropbox/Apps/Overleaf/Thesis/Tables")




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

# Write the package info to HTML file on github
grateful::cite_packages(output = "file",
                        out.dir = "~/Git/matrix-completion",
                        out.format = "pdf",
                        omit = NULL,
                        cite.tidyverse = T,
                        dependencies = T,
                        include.RStudio = T)
file.remove("grateful-refs.bib") # cleanup
file.rename("grateful-report.pdf", "Packages.pdf")

## Modify all entries to have type "Manual"
# Read file
bib_file = "/Users/ts/Library/CloudStorage/Dropbox/Apps/Overleaf/Thesis/Tex/packages.bib"
bib_entries = readLines(bib_file)
# Modify the entry types to "Manual"
modified_entries = gsub("@[^}]+\\{", "@Manual{", bib_entries)
writeLines(modified_entries, con = bib_file)
