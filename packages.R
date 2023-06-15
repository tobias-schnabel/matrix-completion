# This script loads and installs all packages
# Check package status and versions
if (renv::status()$synchronized == T) {
  writeLines("Loading ...")
} else {
  writeLines("Package Versions DO NOT MATCH renv lockfile")
  renv_bool = utils::menu(c("No (not recommended, installs latest versions)", 
                            "Yes (recommended)"), 
                          title = "Use renv to restore correct versions?" )
  if (renv_bool == 2) {
    renv::restore()
  } else {
    writeLines("Locked versions ignored, installing latest versions")
  }

CRAN_packages = c("renv", "tidyverse", "xtable", "purrr", "knitr", "withr", "lfe",
              "callr", "ggplot2", "scales", "panelView", "devtools","fect", "didimputation",
              "latex2exp",  "gsynth", "did2s", "DIDmultiplegt", "grateful", "parallel",
              "fixest", "DRDID", "staggered", "modelsummary", "ggthemes", "fastDummies")

github_packages = c("MCPanel", "synthdid", "MarcusSantAnna2020") # not available on CRAN
github_repos = c("susanathey/MCPanel", "synth-inference/synthdid", "pedrohcgs/MarcusSantAnna2020")

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
}  

# Check installation status of GitHub packages
if (length(github_packages) > length(installed_packages_gh)) {
  warn2 = writeLines("Would you like to install the current versions of all packages 
available on GitHub? These versions might differ from those provided in 
'renv.lock' (see readme for details) [y/N]:")
  
  gh_bool = utils::menu(c("No (recommended)", "Yes"), title = warn2)
  if (gh_bool == 2) {
    devtools::install_github(github_repos[!installed_packages_gh])
  }
}

#Clean up global environment
base::remove(CRAN_packages, github_packages, installed_packages_cran,
             installed_packages_gh)
} 

## Load packages 
  # There are more efficient ways of loading these, but the manual library
  # statements are required for renv to detect each package
  suppressMessages(suppressWarnings(require(renv)))
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
  suppressMessages(suppressWarnings(library(modelsummary)))
  suppressMessages(suppressWarnings(library(kableExtra)))
  suppressMessages(suppressWarnings(library(fect)))
  suppressMessages(suppressWarnings(library(knitr)))
  suppressMessages(suppressWarnings(library(grateful)))
  suppressMessages(suppressWarnings(library(didimputation)))
  suppressMessages(suppressWarnings(library(withr)))
  suppressMessages(suppressWarnings(library(scales)))
  suppressMessages(suppressWarnings(library(ggthemes)))
  suppressMessages(suppressWarnings(library(lfe)))
  suppressMessages(suppressWarnings(library(parallel)))
  suppressMessages(suppressWarnings(library(fastDummies)))
  writeLines("All required CRAN packages are loaded")

  library(MCPanel)
  library(synthdid)
  library(MarcusSantAnna2020)
  writeLines("All required GitHub packages are loaded")
  writeLines("Ready")
