# This script lists and installs all packages
CRAN_packages = c("tidyverse", "xtable", "stargazer", "purrr", 
              "callr", "ggplot2", "panelView", "devtools", 
              "latex2exp",  "gsynth", "did2s", "DIDmultiplegt",
              "fixest", "DRDID", "staggered")

github_packages = c("MCPanel", "synthdid") # not available on CRAN
installed_packages_cran = CRAN_packages %in% rownames(installed.packages())
installed_packages_gh = github_packages %in% rownames(installed.packages())

# Check installation status of CRAN packages
if (length(CRAN_packages) > length(installed_packages_cran)) {
  warn = writeLines("Would you like to install the current versions of all packages 
available on CRAN? These versions might differ from those provided in 
'renv.lock' (see readme for details) [y/N]:")
  
  cran_bool = utils::menu(c("No (recommended)", "Yes"), title = warn)
  if (cran_bool == 2) {
    install.packages(CRAN_packages[!installed_packages_cran])  
  }
} else {
  # There are more efficient ways of loading these, but the manual library
  # statements are required for renv
  suppressMessages(suppressWarnings(library(tidyverse)))
  suppressMessages(suppressWarnings(library(xtable)))
  suppressMessages(suppressWarnings(library(stargazer)))
  suppressMessages(suppressWarnings(library(purrr)))
  suppressMessages(suppressWarnings(library(callr)))
  suppressMessages(suppressWarnings(library(ggplot2)))
  suppressMessages(suppressWarnings(library(latex2exp)))
  suppressMessages(suppressWarnings(library(devtools)))
  suppressMessages(suppressWarnings(library(panelView)))
  suppressMessages(suppressWarnings(library(gsynth)))
  suppressMessages(suppressWarnings(library(did2s)))
  suppressMessages(suppressWarnings(library(DIDmultiplegt)))
  suppressMessages(suppressWarnings(library(fixest)))
  suppressMessages(suppressWarnings(library(DRDID)))
  suppressMessages(suppressWarnings(library(staggered)))
  writeLines("All required CRAN packages are loaded")
}

# Check installation status of CRAN packages
if (length(github_packages) > length(installed_packages_gh)) {
  warn2 = writeLines("Would you like to install the current versions of all packages 
available on GitHub? These versions might differ from those provided in 
'renv.lock' (see readme for details) [y/N]:")
  
  gh_bool = utils::menu(c("No (recommended)", "Yes"), title = warn2)
  if (gh_bool == 2) {
    devtools::install_github(github_packages[!installed_packages_gh])
  }
} else {
  library(MCPanel)
  library(synthdid)
  writeLines("All required GitHub packages are loaded")
}

#Clean up global environment
remove(CRAN_packages, github_packages, installed_packages_cran,
       installed_packages_gh)
